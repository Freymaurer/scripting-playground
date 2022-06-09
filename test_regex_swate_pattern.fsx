#r "nuget: Expecto, 9.0.4"

module Pattern =

    [<LiteralAttribute>]
    let HashNumberPattern = "#\d+"

    [<LiteralAttribute>]
    // This pattern captures all characters between squared brackets (with squared brackets).
    let SquaredBracketsPattern = "\[.*\]"

    [<LiteralAttribute>]
    // This pattern captures all characters between brackets (with brackets).
    let BracketsPattern = "\([^\]]*\)"

    [<LiteralAttribute>]
    let DoubleQuotesPattern = "\"(.*?)\""

    [<LiteralAttribute>]
    // This pattern captures all input coming before an opening square bracket or normal bracket (with whitespace).
    let CoreNamePattern = "^[^[(\n]*"

    // Hits: ENVO:01001831
    [<LiteralAttribute>]
    let TermAccessionPattern = "[a-zA-Z0-9]+?[:_][a-zA-Z0-9]+"


module LukasPattern =
    let kindPattern = @".*(?= [\[\(])"
    let namePattern = @"(?<= \[)[^#\]]*(?=[\]#])"
    let ontologySourcePattern = @"(?<=\()\S+:[^;)#]*(?=[\)\#])"
    let numberPattern = @"(?<=#)\d+(?=[\)\]])"

module TestCases =
    let case1 = "Source Name"
    let case2 = "Sample Name"
    let case3 = "Characteristics [Sample type]"
    let case4 = "Characteristics [biological replicate]"
    let case5 = "Factor [Sample type#2]"
    let case6 = "Parameter [biological replicate#2]"
    let case7 = "Data File Name"
    let case8 = "Term Source REF (NFDI4PSO:0000064)"
    let case9 = "Term Source REF (NFDI4PSO:0000064#2)"
    let case10 = "Term Accession Number (MS:1001809)"
    let case11 = "Term Accession Number (MS:1001809#2)"
    let case12 = "Unit"
    let case13 = "Unit (#3)"
    let case14 = "Term Accession Number ()" 

module UnitTests =
    open Expecto
    open System.Text.RegularExpressions

    let patternTests =
        testList "Regex patterns" [
            test "CoreNamePattern 'Source Name'" {
                let regExMatch = Regex.Match(TestCases.case1, Pattern.CoreNamePattern)
                let regexValue = regExMatch.Value
                Expect.equal regexValue "Source Name" ""
            }

            test "CoreNamePattern 'Characteristic [Sample type]'" {
                let regExMatch = Regex.Match(TestCases.case3, Pattern.CoreNamePattern)
                let regexValue = regExMatch.Value.Trim()
                Expect.equal regexValue "Characteristics" ""
            }

            test "CoreNamePattern 'Term Source REF (NFDI4PSO:0000064)'" {
                let regExMatch = Regex.Match(TestCases.case8, Pattern.CoreNamePattern)
                let regexValue = regExMatch.Value.Trim()
                Expect.equal regexValue "Term Source REF" ""
            }

            test "CoreNamePattern 'Term Accession Number (MS:1001809#2)'" {
                let regExMatch = Regex.Match(TestCases.case11, Pattern.CoreNamePattern)
                let regexValue = regExMatch.Value.Trim()
                Expect.equal regexValue "Term Accession Number" ""
            }

            test "CoreNamePattern 'Unit (#3)'" {
                let regExMatch = Regex.Match(TestCases.case13, Pattern.CoreNamePattern)
                let regexValue = regExMatch.Value.Trim()
                Expect.equal regexValue "Unit" ""
            }

            test "CoreNamePattern 'Term Accession Number ()'" {
                let regExMatch = Regex.Match(TestCases.case14, Pattern.CoreNamePattern)
                let regexValue = regExMatch.Value.Trim()
                Expect.equal regexValue "Term Accession Number" ""
            }
        ]
    Expecto.Tests.runTests Impl.ExpectoConfig.defaultConfig patternTests

