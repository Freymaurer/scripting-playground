#r "nuget: DynamicObj, 1.0.1"

// type Modifier = 
//     {
//         Name:           string
//         AttackBonus:    Bonus
//         Damage:         ComplexTypes.Damage
//         BonusAttacks:   ComplexTypes.BonusAttacks
//         SizeChange:     ComplexTypes.SizeChange
//         Description:    string
// }

open DynamicObj

type Modifer(?props) =
    inherit DynamicObj()

let calculatePowerAttackMalus = fun (bab:obj) -> -1. - floor ((string >> float) bab/4.)

/// Set Bab for automated calculation
let powerAttack(props) = 
    let m = Modifer()
    let bab = m.TryGetValue "bab"
    printfn "%A" bab
    m.SetValue("AttackBonus", if bab.IsSome then calculatePowerAttackMalus bab.Value else calculatePowerAttackMalus (box 0))
    m

let pa = powerAttack()

pa.SetValue("bab", 10)

pa.TryGetValue "AttackBonus"

calculatePowerAttackMalus (box "9")