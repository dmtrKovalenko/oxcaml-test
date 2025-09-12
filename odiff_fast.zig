const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

// Pixel structure with RGBA components as unboxed floats (equivalent to OxCaml float#)
// Using packed struct to ensure tight memory layout like OxCaml's unboxed types
const Pixel = packed struct {
    r: f64,
    g: f64, 
    b: f64,
    a: f64,
};

// Constants for color space conversions (precomputed for optimization)
const Y_R_COEFF: f64 = 0.29889531;
const Y_G_COEFF: f64 = 0.58662247;
const Y_B_COEFF: f64 = 0.11448223;
const I_R_COEFF: f64 = 0.59597799;
const I_G_COEFF: f64 = -0.27417610;
const I_B_COEFF: f64 = -0.32180189;
const Q_R_COEFF: f64 = 0.21147017;
const Q_G_COEFF: f64 = -0.52261711;
const Q_B_COEFF: f64 = 0.31114694;

// White pixel constant
const WHITE_PIXEL = Pixel{ .r = 255.0, .g = 255.0, .b = 255.0, .a = 0.0 };

// Aggressive inlining for hot functions - blend channel with white background
inline fn blendChannelWhite(color: f64, alpha: f64) f64 {
    return 255.0 + ((color - 255.0) * alpha);
}

// Blend semi-transparent pixel with white background
inline fn blendSemiTransparentPixel(pixel: Pixel) Pixel {
    if (pixel.a == 0.0) {
        return WHITE_PIXEL;
    } else if (pixel.a == 255.0) {
        return Pixel{ .r = pixel.r, .g = pixel.g, .b = pixel.b, .a = 1.0 };
    } else if (pixel.a < 255.0) {
        const normalizedAlpha = pixel.a / 255.0;
        return Pixel{
            .r = blendChannelWhite(pixel.r, normalizedAlpha),
            .g = blendChannelWhite(pixel.g, normalizedAlpha),
            .b = blendChannelWhite(pixel.b, normalizedAlpha),
            .a = normalizedAlpha,
        };
    } else {
        @panic("Found pixel with alpha value greater than uint8 max value. Aborting.");
    }
}

// Decode raw 32-bit pixel value to Pixel struct
inline fn decodeRawPixel(pixel: u32) Pixel {
    const a: f64 = @floatFromInt((pixel >> 24) & 0xFF);
    const b: f64 = @floatFromInt((pixel >> 16) & 0xFF);
    const g: f64 = @floatFromInt((pixel >> 8) & 0xFF);
    const r: f64 = @floatFromInt(pixel & 0xFF);
    
    return Pixel{ .r = r, .g = g, .b = b, .a = a };
}

// Color space conversions optimized for FMA (Fused Multiply-Add)
inline fn rgb2y(pixel: Pixel) f64 {
    return (pixel.r * Y_R_COEFF) + (pixel.g * Y_G_COEFF) + (pixel.b * Y_B_COEFF);
}

inline fn rgb2i(pixel: Pixel) f64 {
    return (pixel.r * I_R_COEFF) + (pixel.g * I_G_COEFF) + (pixel.b * I_B_COEFF);
}

inline fn rgb2q(pixel: Pixel) f64 {
    return (pixel.r * Q_R_COEFF) + (pixel.g * Q_G_COEFF) + (pixel.b * Q_B_COEFF);
}

// Main calculation structured for vectorization
inline fn calculatePixelColorDelta(pixelA: u32, pixelB: u32) f64 {
    const decodedA = blendSemiTransparentPixel(decodeRawPixel(pixelA));
    const decodedB = blendSemiTransparentPixel(decodeRawPixel(pixelB));

    // Compute all color space coordinates
    const y_a = rgb2y(decodedA);
    const i_a = rgb2i(decodedA);
    const q_a = rgb2q(decodedA);
    const y_b = rgb2y(decodedB);
    const i_b = rgb2i(decodedB);
    const q_b = rgb2q(decodedB);

    // Compute differences
    const y_diff = y_a - y_b;
    const i_diff = i_a - i_b;
    const q_diff = q_a - q_b;

    // Weighted sum of squares - pattern that vectorizes well
    return (0.5053 * y_diff * y_diff) + (0.299 * i_diff * i_diff) + (0.1957 * q_diff * q_diff);
}

inline fn calculatePixelBrightnessDelta(pixelA: u32, pixelB: u32) f64 {
    const decodedA = blendSemiTransparentPixel(decodeRawPixel(pixelA));
    const decodedB = blendSemiTransparentPixel(decodeRawPixel(pixelB));
    return rgb2y(decodedA) - rgb2y(decodedB);
}

// Vectorization-friendly batch processing with loop unrolling
noinline fn calculateBatchColorDeltas(allocator: Allocator, pixelsA: []const u32, pixelsB: []const u32) ![]f64 {
    const len = pixelsA.len;
    std.debug.assert(len == pixelsB.len);
    
    var results = try allocator.alloc(f64, len);
    
    // Unroll by 8 for better SIMD utilization
    var i: usize = 0;
    while (i + 7 < len) {
        // Process 8 pixels at once - optimal for 256-bit SIMD
        results[i] = calculatePixelColorDelta(pixelsA[i], pixelsB[i]);
        results[i + 1] = calculatePixelColorDelta(pixelsA[i + 1], pixelsB[i + 1]);
        results[i + 2] = calculatePixelColorDelta(pixelsA[i + 2], pixelsB[i + 2]);
        results[i + 3] = calculatePixelColorDelta(pixelsA[i + 3], pixelsB[i + 3]);
        results[i + 4] = calculatePixelColorDelta(pixelsA[i + 4], pixelsB[i + 4]);
        results[i + 5] = calculatePixelColorDelta(pixelsA[i + 5], pixelsB[i + 5]);
        results[i + 6] = calculatePixelColorDelta(pixelsA[i + 6], pixelsB[i + 6]);
        results[i + 7] = calculatePixelColorDelta(pixelsA[i + 7], pixelsB[i + 7]);
        i += 8;
    }
    
    // Handle remaining pixels
    while (i < len) {
        results[i] = calculatePixelColorDelta(pixelsA[i], pixelsB[i]);
        i += 1;
    }
    
    return results;
}

// Accumulation using Kahan summation for better precision
fn calculateImageDifference(imageA: []const u32, imageB: []const u32) f64 {
    const len = imageA.len;
    std.debug.assert(len == imageB.len);
    
    var sum: f64 = 0.0;
    var c: f64 = 0.0; // Compensation for lost low-order bits
    
    for (0..len) |i| {
        const delta = calculatePixelColorDelta(imageA[i], imageB[i]);
        const y = delta - c;
        const t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    
    return sum / @as(f64, @floatFromInt(len));
}

// Enhanced benchmark with performance metrics
fn runBenchmark(allocator: Allocator) !void {
    const pixelA: u32 = 0xFF33A1CE;
    const pixelB: u32 = 0xFF33A1CF; // Slightly different for non-zero delta
    
    print("=== Zig High-Performance Image Difference ===\n", .{});
    
    // Single pixel test
    const delta = calculatePixelColorDelta(pixelA, pixelB);
    print("Single pixel delta: {d:.6}\n", .{delta});
    
    // Multi-size benchmarks
    const test_sizes = [_]usize{ 1000, 10000, 100000, 1000000 };
    
    for (test_sizes) |test_size| {
        const imageA = try allocator.alloc(u32, test_size);
        defer allocator.free(imageA);
        const imageB = try allocator.alloc(u32, test_size);
        defer allocator.free(imageB);
        
        // Fill arrays with test data
        @memset(imageA, pixelA);
        @memset(imageB, pixelB);
        
        print("\nProcessing {d} pixels:\n", .{test_size});
        
        // Batch processing benchmark
        const start_time = std.time.milliTimestamp();
        const batch_results = try calculateBatchColorDeltas(allocator, imageA, imageB);
        defer allocator.free(batch_results);
        const end_time = std.time.milliTimestamp();
        
        const processing_time = @as(f64, @floatFromInt(end_time - start_time)) / 1000.0;
        const pixels_per_second = @as(f64, @floatFromInt(test_size)) / processing_time;
        
        print("  Batch time: {d:.6} seconds\n", .{processing_time});
        print("  Throughput: {d:.0} pixels/second\n", .{pixels_per_second});
        print("  Performance: {d:.2} MPix/s\n", .{pixels_per_second / 1_000_000.0});
        
        // Image difference test
        const start_time2 = std.time.milliTimestamp();
        const img_diff = calculateImageDifference(imageA, imageB);
        const end_time2 = std.time.milliTimestamp();
        
        const img_diff_time = @as(f64, @floatFromInt(end_time2 - start_time2)) / 1000.0;
        print("  Image diff time: {d:.6} seconds\n", .{img_diff_time});
        
        // Calculate average delta
        var sum: f64 = 0.0;
        for (batch_results) |result| {
            sum += result;
        }
        const avg_delta = sum / @as(f64, @floatFromInt(test_size));
        
        print("  Average delta: {d:.6}\n", .{avg_delta});
        print("  Image difference: {d:.6}\n", .{img_diff});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    try runBenchmark(allocator);
}
