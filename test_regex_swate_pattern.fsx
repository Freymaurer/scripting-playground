module Pattern =

    [<LiteralAttribute>]
    let HashNumberPattern = "#\d+"

    [<LiteralAttribute>]
    /// This pattern captures all characters between squared brackets (with squared brackets).
    let SquaredBracketsPattern = "\[.*\]"

    [<LiteralAttribute>]
    /// This pattern captures all characters between brackets (with brackets).
    let BracketsPattern = "\([^\]]*\)"

    [<LiteralAttribute>]
    let DoubleQuotesPattern = "\"(.*?)\""

    [<LiteralAttribute>]
    /// This pattern captures all input coming before an opening square bracket or normal bracket (with whitespace).
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

//Source Name
//Sample Name
//Characteristics [Sample type]
//Characteristics [biological replicate]
//Factor [Sample type#2]
//Parameter [biological replicate#2]
//Data File Name
//Term Source REF (NFDI4PSO:0000064)
//Term Source REF (NFDI4PSO:0000064#2)
//Term Accession Number (MS:1001809)
//Term Accession Number (MS:1001809#2)
//Unit
//Unit (#3)


let t1 = "1"
let t2 = "1","2"
let t3 = "1","2","3"

let matching (level:int) =
    match level with
    | 1 -> t1
    | 2 -> $"{fst t2}, {snd t2}"
    | 3 -> sprintf "%A" t3

matching 3