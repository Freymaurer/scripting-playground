#r "nuget: FSharp.Stats"
#r "nuget: Expecto, 9.0.4"

open Expecto
open FSharp.Stats

[<Literal>]
let test_basicNumber = 0.42

let bernoulliDistribution_basicCase = Distributions.Discrete.bernoulli test_basicNumber
let bernoulliDistribution_nan = Distributions.Discrete.bernoulli nan
let bernoulliDistribution_zero = Distributions.Discrete.bernoulli 0.0
let bernoulliDistribution_one = Distributions.Discrete.bernoulli 1.0

bernoulliDistribution_nan

nan >= 0

[<Tests>]
let bernoulliTests =
    // 2022-06-22
    // Wikipedia: https://de.wikipedia.org/wiki/Bernoulli-Verteilung#Definition 
    // "p is element of closed intervall between 0. and 1."
    testList "Distributions.Discrete.Bernoulli" [
        test "bernCheckParam" {
            let test_lowerThan0 = fun (x: unit) -> Distributions.Discrete.bernCheckParam -0.1
            let test_highterThan1 = fun (x: unit) -> Distributions.Discrete.bernCheckParam 1.1
            let test_basic = Distributions.Discrete.bernCheckParam test_basicNumber
            let test_zero = Distributions.Discrete.bernCheckParam 0.
            let test_one = Distributions.Discrete.bernCheckParam 1.
            let test_nan = Distributions.Discrete.bernCheckParam nan // 
            let test_infinity = fun (x: unit) -> Distributions.Discrete.bernCheckParam infinity
            let test_negativeInfinity = fun (x: unit) -> Distributions.Discrete.bernCheckParam -infinity
            Expect.throws test_lowerThan0 ""
            Expect.throws test_highterThan1 ""
            Expect.equal test_basic () ""
            Expect.equal test_zero () ""
            Expect.equal test_one () ""
            Expect.equal test_nan () ""
            Expect.throws test_infinity ""
            Expect.throws test_negativeInfinity ""
        }

        test "Mean" {
            Expect.equal bernoulliDistribution_basicCase.Mean test_basicNumber ""
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.Mean)) ""
            Expect.equal bernoulliDistribution_zero.Mean 0.0 ""
            Expect.equal bernoulliDistribution_one.Mean 1.0 ""
        }
        // 2022-06-22
        // Compared to: https://www.trignosource.com/statistics/bernoulli%20distribution.html
        test "Variance" {
            Expect.equal bernoulliDistribution_basicCase.Variance 0.2436 ""
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.Variance)) ""
            Expect.equal bernoulliDistribution_zero.Variance 0.0 ""
            Expect.equal bernoulliDistribution_one.Variance 0.0 ""
        }
        // 2022-06-22
        // Compared to: https://www.trignosource.com/statistics/bernoulli%20distribution.html
        // https://www.kristakingmath.com/blog/bernoulli-random-variables
        test "StandardDeviation" {
            Expect.equal bernoulliDistribution_basicCase.StandardDeviation (sqrt 0.2436) ""
            Expect.isTrue (nan.Equals(bernoulliDistribution_nan.StandardDeviation)) ""
            Expect.equal bernoulliDistribution_zero.StandardDeviation (sqrt 0.0) ""
            Expect.equal bernoulliDistribution_one.StandardDeviation (sqrt 0.0) ""
        }
        // not implemented
        test "Sample" {
            Expect.throws (bernoulliDistribution_basicCase.Sample >> ignore) ""
            Expect.throws (bernoulliDistribution_nan.Sample >> ignore) ""
            Expect.throws (bernoulliDistribution_zero.Sample >> ignore) ""
            Expect.throws (bernoulliDistribution_one.Sample >> ignore)  ""
        }
        // **Fsharps.Stats bug, needed to move test dev directly into repo**
        test "PDF" {
            /// propabiliy of an outcome to be be of a certain value. Bernoulli distribution can only result in 0 (failure) or 1 (success) so anything except 
            /// those should have a propability of 0.
            let testZeroAndOne (bd: Distributions.Distribution<float,float>) = 
                let propabilitySuccess = bd.PDF 1.0
                let propabilityFailure = bd.PDF 0.0
                Expect.equal propabilitySuccess (bd.Mean) $"testZeroAndOne.propabilitySuccess for {bd.Mean}"
                Expect.floatClose Accuracy.high propabilityFailure (1.0 - bd.Mean) $"testZeroAndOne.propabilityFailure for {bd.Mean}"
            let testAllZeroPDFCases (bd: Distributions.Distribution<float,float>) =
                Expect.equal (bd.PDF 0.1) 0.0 $"testAllZeroPDFCases 0.1 for {bd.Mean}"
                Expect.equal (bd.PDF -0.1) 0.0 $"testAllZeroPDFCases -0.1 for {bd.Mean}"
                Expect.equal (bd.PDF 1.1) 0.0 $"testAllZeroPDFCases 1.1 for {bd.Mean}"
                Expect.equal (bd.PDF nan) 0.0 $"testAllZeroPDFCases nan for {bd.Mean}"
                Expect.equal (bd.PDF infinity) 0.0 $"testAllZeroPDFCases infinity for {bd.Mean}"
                Expect.equal (bd.PDF -infinity) 0.0 $"testAllZeroPDFCases -infinity for {bd.Mean}"
            testAllZeroPDFCases bernoulliDistribution_basicCase
            testAllZeroPDFCases bernoulliDistribution_nan
            testAllZeroPDFCases bernoulliDistribution_zero
            testAllZeroPDFCases bernoulliDistribution_one
            // testZeroAndOne bernoulliDistribution_basicCase
            // Expect.isTrue (nan.Equals(bernoulliDistribution_nan.PDF 0.0)) $""
            // Expect.isTrue (nan.Equals(bernoulliDistribution_nan.PDF 1.0)) $""
            // testZeroAndOne bernoulliDistribution_zero
            // testZeroAndOne bernoulliDistribution_one
        }
        // **Fsharps.Stats bug, needed to move test dev directly into repo**
        test "CDF" {
            // For P(x>=R) and R∈{0,1}, where R is the random outcome of the bernoulli distribution, any value below 0 has a probability of 0 to be greater or equal to R
            let testAllZeroCDFCases (bd: Distributions.Distribution<float,float>) = 
                Expect.equal (bd.CDF -0.1) 0.0 $"testAllZeroCDFCases -0.1 for {bd.Mean}"
                Expect.equal (bd.CDF -infinity) 0.0 $"testAllZeroCDFCases -infinity for {bd.Mean}"
            testAllZeroCDFCases bernoulliDistribution_basicCase
            testAllZeroCDFCases bernoulliDistribution_nan
            testAllZeroCDFCases bernoulliDistribution_zero
            testAllZeroCDFCases bernoulliDistribution_one
        }
    ]

Expecto.Tests.runTestsWithCLIArgs [] [||] bernoulliTests |> ignore