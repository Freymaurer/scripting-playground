open System

let random = System.Random()

let createNewRandom() =
    let seeding() = System.Random().Next(0,10)
    let seed = Array.init 8 (fun _ -> string <| seeding()) |> String.concat "" |> int
    System.Random(seed)

let shuffle (org:_[]) = 
    let random = createNewRandom()
    let arr = Array.copy org
    let max = (arr.Length - 1)
    let randomSwap (arr:_[]) i =
        let pos = random.Next(max+1)
        let tmp = arr.[pos]
        arr.[pos] <- arr.[i]
        arr.[i] <- tmp
        arr
   
    [|0..max|] |> Array.fold randomSwap arr

module Dice = 
    let d(max:int) = random.Next(1,max+1)
    let d3() = random.Next(1,4)
    let d4() = random.Next(1,5)
    let d6() = random.Next(1,7)
    let d8() = random.Next(1,9)
    let d10() = random.Next(1,11)
    let d12() = random.Next(1,13)
    let d20() = random.Next(1,21)

type AbilityScore =
| Str
| Con
| Dex
| Wis
| Int
| Cha
/// Use this for flexible boni on human and half-human races
| Any
with
    static member (?/) (abilityScore1:AbilityScore,abilityScore2:AbilityScore) = 
        shuffle [|abilityScore1; abilityScore2|]

    static member (?/) (abilityScoreArr:AbilityScore [],abilityScore2:AbilityScore) = 
        shuffle [| 
            yield! abilityScoreArr
            abilityScore2
        |] 

type Subschools =
// Abjuration
| Banishment
| Counterspell
// Conjuaration
| Creation
| Extradimension
| Infernal_Binder
| Teleportation
// Divination
| Prophecy
| Foresight
| Scryer
// Enchantment
| Controller
| Manipulator
// Evocation
| Admixture
| Generation
// Illusion
| Deception
| Mage_of_the_Veil
| Phantasm
| Shadow
// Necromancy
| Life
| Undead
// Transmutation
| Enhancement
| Shapechange
// Universalist
| Arcane_Crafter

type MagicSchools =
| Abjuration
| Conjuration
| Divination
| Enchantment
| Evocation
| Illusion
| Necromancy        
| Sin_Magic
| Transmutation
| Universalist

with
    member this.Subschools =
        match this with
        | Abjuration    -> [|Banishment; Counterspell|]
        | Conjuration   -> [|Creation; Extradimension; Infernal_Binder; Teleportation|]
        | Divination    -> [|Prophecy; Foresight; Scryer|]
        | Enchantment   -> [|Controller; Manipulator|]
        | Evocation     -> [|Admixture; Generation|]
        | Illusion      -> [|Deception; Mage_of_the_Veil; Phantasm; Shadow|]
        | Necromancy    -> [|Life; Undead|]
        | Sin_Magic     -> [||]
        | Transmutation -> [|Enhancement; Shapechange|]
        | Universalist  -> [|Arcane_Crafter|]

    static member collection = [|
        Abjuration
        Conjuration
        Divination
        Enchantment
        Evocation
        Illusion
        Necromancy
        Sin_Magic
        Transmutation
        Universalist
    |]


type PlaystyleArchetypes =    
/// Caster of School
| Caster of (MagicSchools*Subschools option)
| Ranged
| MeleeStrength
| MeleeDex
with
    static member initCaster() = 
        let random = createNewRandom()
        let rndMagicSchool = 
            let schools = MagicSchools.collection
            schools.[random.Next(0,schools.Length)]
        let hasSubschool =
            /// Get all subschools and create 50% chance to get no subschool
            let subschools = Sin_Magic.Subschools |> Array.collect (fun x -> [|None; Some x|])
            if subschools |> Array.isEmpty then 
                None 
            else 
                subschools.[random.Next(0,subschools.Length)]
        Caster (rndMagicSchool, hasSubschool)

type BABProgression =
| Slow
| Medium
| Full
    member this.ofLevel (lvl:int) =
        match this with
        | Slow      -> 10./20.
        | Medium    -> 15./20.
        | Full      -> 20./20.
        |> (*) (float lvl)
        |> floor 
        |> int

// type Prerequisites = {
//     BAB: int option
//     Class: string option
//     ClassFeatures: string []
//     Feats: Feat []
// }

// and Feat = {
//     /// Prerequisites
//     Prerequisites: Prerequisites
//     Playstyle: PlaystyleArchetypes
//     URL: System.Uri
// }

type Playstyle = {
    PlaystyleArchetype: PlaystyleArchetypes
    AbilityScoreOrder: AbilityScore []
    // MustHaveFeats: Feat []
} with 
    static member create archetype abilityscores = {
        PlaystyleArchetype = archetype
        AbilityScoreOrder = abilityscores
    }

type Class ={
    Name                : string
    Playstyle           : Playstyle
    BABProgression      : BABProgression
    HitDie              : int
    // MustHaveFeats       : Feat []
}


module AbilityScores = 

    let private pointBuy20Presets = 
        [|
            [|15; 15; 12; 12; 12; 10 |]
            [|15; 15; 14; 14; 10; 7  |]
            [|16; 16; 10; 10; 10; 10 |]
            [|16; 15; 14; 12; 10; 7  |]
            [|16; 14; 14; 12; 10; 8  |]
            [|16; 14; 14; 10; 10; 10 |]
            [|16; 14; 13; 12; 10; 10 |]
            [|16; 14; 12; 12; 11; 10 |]
            [|16; 12; 12; 12; 12; 12 |]
            [|17; 17; 12; 10; 7;  7  |]
            [|17; 16; 14; 10; 7;  7  |]
            [|17; 16; 11; 10; 10; 7  |]
            [|17; 16; 12; 10; 9;  7  |]
            [|17; 15; 12; 12; 10; 7  |]
            [|18; 15; 12; 12; 7;  7  |]
            [|18; 15; 12; 12; 7;  7  |]
            [|18; 14; 14; 11; 7;  7  |]
            [|18; 14; 13; 13; 7;  7  |]
            [|18; 14; 12; 12; 8;  7  |]
            [|18; 13; 12; 12; 10; 7  |]
            [|18; 14; 12; 12; 8;  8  |]
        |] |> Array.distinct

    let scoreToPointBuy(score:int) =
        match score with
        | min when score <= 7 -> -4
        | 8 -> -2
        | 9 -> -1
        | 10 -> 0
        | 11 -> 1
        | 12 -> 2
        | 13 -> 3
        | 14 -> 5
        | 15 -> 7
        | 16 -> 10
        | 17 -> 13
        | max when score >= 18 -> 17
        | _ -> failwith "~This can never be hit and just disables compiler warning.~"

    let inline avg (seq:seq<'a>) =
        let n = seq |> Seq.length |> float
        let sum = seq |> Seq.sum |> float
        sum/n

    open Dice

    type AbilityScoreGen =
    /// 4d6 discard lowest;
    /// Avg. ability score over 100k = 12.24348667;
    /// Avg. point buy in over 1mio (with score >= 7 equals -4) = 19.469128
    | Standard
    /// 3d6;
    /// Avg. over 100k = 10.49920333;
    /// Avg. point buy in over 1mio (with score >= 7 equals -4) = 5.390454
    | Classic
    /// 2d6 + 6;
    /// Avg. over 100k = 12.99776333
    /// Avg. point buy in over 1mio (with score >= 7 equals -4) = 25.669565
    | Heroic
    /// Random from list of presets
    | PointBuy20
    with
        member this.roller() =
            match this with 
            | Standard  -> 
                let one() = Array.init 4 (fun e -> d6()) |> fun a -> Array.sum a - (Array.min a)
                Array.init 6 (fun e -> one() )
            | Classic   -> 
                let one() = Array.init 3 (fun e -> d6()) |> Array.sum
                Array.init 6 (fun e -> one() )
            | Heroic    ->
                let one() = Array.init 2 (fun e -> d6()) |> Array.sum |> (+) 6
                Array.init 6 (fun e -> one() )
                |> fun x -> x
            | PointBuy20 ->
                let i = random.Next(0, pointBuy20Presets.Length)
                pointBuy20Presets.[i]

module CharacterClasses =

    module Core =
    
        let FighterRanged() = {
            Name                = "Fighter"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create Ranged [|Dex; Str; Con; yield! Wis ?/ Int ?/ Cha|]
        }

        let FighterMeleeStr() = {
            Name               = "Fighter"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create MeleeStrength [|Str; yield! Dex ?/ Con; yield! Wis ?/ Int ?/ Cha|]
        }

        let FighterMeleeDex() = {
            Name               = "Fighter"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create MeleeDex [|Dex; yield! Str ?/ Con; yield! Wis ?/ Int ?/ Cha|]
        }

        let BarbarianMeleeStr() = {
            Name               = "Barbarian"
            BABProgression      = Full
            HitDie              = 12
            Playstyle           = Playstyle.create MeleeDex [|Dex; yield! Str ?/ Con; yield! Wis ?/ Int ?/ Cha|]
        }

        let BardCaster() = {
            Name               = "Bard"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.initCaster())  [|Cha; Dex; yield! Int ?/ Con; Wis; Str|]
        }

        let BardMeleeDex() = {
            Name               = "Bard"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create PlaystyleArchetypes.MeleeDex [|Dex; yield! Cha ?/ Con; Int; Wis; Str|]
        }

        let BardRangedDex() = {
            Name               = "Bard"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create PlaystyleArchetypes.Ranged [|Dex; Cha; Int; Con; Wis; Str|]
        }

        let ClericCaster() = {
            Name               = "Cleric"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.initCaster()) [|Wis; yield! Con ?/ Dex ?/ Cha ?/ Int ?/ Str|]
        }

        let ClericMeleeStr() = {
            Name               = "Cleric"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.MeleeStrength) [|Str; yield! Con ?/ Dex ?/ Wis; Cha; Int|]
        }

        let DruidMeleeStr() = {
            Name               = "Druid"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.MeleeStrength) [|Str; yield! Con ?/ Dex ?/ Wis; yield! Cha ?/ Int|]
        }

        let DruidCaster() = {
            Name               = "Druid"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.initCaster()) [|Wis; yield! Con ?/ Dex; yield! Cha ?/ Int ?/ Str|]
        }

        let MonkMeleeStr() = {
            Name               = "Monk"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.MeleeStrength) [|Str; yield! Dex ?/ Con ?/ Wis; yield! Int ?/ Cha|]
        }

        let MonkMeleeDex() = {
            Name               = "Monk"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.MeleeDex) [|Dex; yield! Con ?/ Wis; yield! Str ?/ Int ?/ Cha|]
        }

        let MonkRanged() = {
            Name               = "Monk"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create (PlaystyleArchetypes.MeleeDex) [|Dex; Str; Wis; Con; yield! Int ?/ Cha|]
        }

        let PaladinMeleeStr() = {
            Name               = "Paladin"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create (PlaystyleArchetypes.MeleeStrength) [|Str; yield! Dex ?/ Con ?/ Cha; yield! Int ?/ Wis|]
        }

        let PaladinRanged() = {
            Name               = "Paladin"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create (PlaystyleArchetypes.Ranged) [|Dex; yield! Str ?/ Cha; Con; yield! Int ?/ Wis|]
        }

        let RangerRanged() = {
            Name                = "Ranger"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create Ranged [|Dex; Str; yield! Wis ?/ Con; yield! Int ?/ Cha|]
        }

        let RangerMeleeStr() = {
            Name               = "Ranger"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create MeleeStrength [|Str; Dex; yield! Con ?/ Wis; yield! Int ?/ Cha|]
        }

        let RangerMeleeDex() = {
            Name               = "Ranger"
            BABProgression      = Full
            HitDie              = 10
            Playstyle           = Playstyle.create MeleeDex [|Dex; Con; yield! Wis ?/ Str; yield! Int ?/ Cha|]
        }

        let RogueMeleeDex() = {
            Name               = "Rogue"
            BABProgression      = Medium
            HitDie              = 8
            Playstyle           = Playstyle.create MeleeDex [|Dex; Con; yield! Wis ?/ Int ?/ Cha; Str|]
        }

        let SorcererMeleeStr() = {
            Name               = "Sorcerer"
            BABProgression      = Slow
            HitDie              = 6
            Playstyle           = Playstyle.create MeleeStrength [|Str; yield! Con ?/ Dex; Cha; Wis; Int|]
        }

        let SorcererCaster() = {
            Name               = "Sorcerer"
            BABProgression      = Slow
            HitDie              = 6
            Playstyle           = Playstyle.create (PlaystyleArchetypes.initCaster()) [|Cha; Dex; yield! Int ?/ Con ?/ Wis; Str|]
        }

        let WizardCaster() = {
            Name               = "Wizard"
            BABProgression      = Slow
            HitDie              = 6
            Playstyle           = Playstyle.create (PlaystyleArchetypes.initCaster()) [|Int; Dex; yield! Cha ?/ Con ?/ Wis; Str|]
        }

        let collection = [|
            FighterRanged; FighterMeleeStr; FighterMeleeDex; BarbarianMeleeStr; BardCaster; BardMeleeDex; BardRangedDex
            ClericCaster; ClericMeleeStr; DruidMeleeStr; DruidCaster; MonkMeleeStr; MonkMeleeDex; MonkRanged
            PaladinMeleeStr; PaladinRanged; RangerRanged; RangerMeleeStr; RangerMeleeDex; RogueMeleeDex
            SorcererMeleeStr; SorcererCaster; WizardCaster
        |]

        let balancedCollection() =
            let groupedCollection = collection |> Array.groupBy (fun c -> c().Name)
            let highestNumberClassArchetypes = groupedCollection |> Array.map (fun (name, l) -> name, l.Length) |> Array.maxBy snd |> snd
            let random = createNewRandom()
            groupedCollection
            |> Array.collect (fun (name, c) ->
                let currentLength = c.Length
                let diff = highestNumberClassArchetypes - currentLength
                [|
                    yield! Array.init diff (fun _ -> c.[random.Next(0,c.Length)]); 
                    yield! c
                |]
    )

module Races =

    type AbilityScoreBonus = {
        AbilityScore: AbilityScore
        Plus_Minus: int
    } with
        static member create(n, abilityScore) = {
            AbilityScore    = abilityScore
            Plus_Minus      = n
        }

    type Race = {
        Name: string
        AbilityScorePlus: AbilityScoreBonus []
        AbilityScoreMinus: AbilityScoreBonus []
    }
    module Core =

        let Dwarf = {
            Name                = "Dwarf"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Con); AbilityScoreBonus.create(+2,Wis)|]
            AbilityScoreMinus   = [|AbilityScoreBonus.create(-2,Cha)|]
        }

        let Elf = {
            Name                = "Elf"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Int); AbilityScoreBonus.create(+2,Dex)|]
            AbilityScoreMinus   = [|AbilityScoreBonus.create(-2,Con)|]
        }

        let Gnome = {
            Name                = "Gnome"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Con); AbilityScoreBonus.create(+2,Cha)|]
            AbilityScoreMinus   = [|AbilityScoreBonus.create(-2,Str)|]
        }

        let Half_Elf = {
            Name                = "Half-Elf"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Any)|]
            AbilityScoreMinus   = [||]
        }

        let Halfling = {
            Name                = "Halfling"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Dex); AbilityScoreBonus.create(+2,Cha)|]
            AbilityScoreMinus   = [|AbilityScoreBonus.create(-2,Str)|]
        }

        let Half_Orc = {
            Name                = "Half-Orc"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Any)|]
            AbilityScoreMinus   = [||]
        }

        let Human = {
            Name                = "Human"
            AbilityScorePlus    = [|AbilityScoreBonus.create(+2,Any)|]
            AbilityScoreMinus   = [||]
        }

        let collection = [|
            Dwarf; Elf; Gnome; Half_Elf; Halfling; Half_Orc; Human
        |]

open Dice
open AbilityScores

type Character = {
    Str     : int
    Dex     : int
    Con     : int
    Int     : int
    Wis     : int
    Cha     : int
    Race    : Races.Race option
    Class   : Class option
    Level   : int
    BAB     : int
} with
    static member init = {
        Str     = 10
        Dex     = 10
        Con     = 10
        Int     = 10
        Wis     = 10
        Cha     = 10
        Race    = None
        Class   = None
        Level   = 1
        BAB     = 0
    }

    member this.getAbilityScore(abilityScore:AbilityScore) =
        match abilityScore with
        | Str -> this.Str
        | Dex -> this.Dex
        | Con -> this.Con
        | Wis -> this.Wis
        | Int -> this.Int
        | Cha -> this.Cha
        | Any -> failwith "Error! Cannot match 'Any' ability score type to character ability score. [Error in 'getAbilityScore']"

    member this.updateAbilityScore(abilityScore:AbilityScore, change:int) =
        match abilityScore with
        | Str -> {this with Str = this.Str + change}
        | Dex -> {this with Dex = this.Dex + change}
        | Con -> {this with Con = this.Con + change}
        | Wis -> {this with Wis = this.Wis + change}
        | Int -> {this with Int = this.Int + change}
        | Cha -> {this with Cha = this.Cha + change}
        | Any -> failwith "Error! Cannot match 'Any' ability score type to character ability score. [Error in 'updateAbilityScore']"

    member this.applyAbilityScores (abilityScores:int []) =
        if abilityScores.Length <> 6 then failwith $"Error. There must be 6 abilityScoreValues, but found '{abilityScores.Length}' in '{abilityScores}'!"
        if this.Class.IsNone then failwith $"Error. Character needs assigned class to use 'applyAbilityScores'!"
        let order = this.Class.Value.Playstyle.AbilityScoreOrder
        if order |> Array.filter (fun x -> x = Str) |> Array.length <> 1 then failwith $"Could not find Strength Score in Ability Score order for class '{this.Class.Value.Name}, {this.Class.Value.Playstyle}'"
        if order |> Array.filter (fun x -> x = Dex) |> Array.length <> 1 then failwith $"Could not find Dexterity Score in Ability Score order for class '{this.Class.Value.Name}, {this.Class.Value.Playstyle}'"  
        if order |> Array.filter (fun x -> x = Con) |> Array.length <> 1 then failwith $"Could not find Constitution Score in Ability Score order for class '{this.Class.Value.Name}, {this.Class.Value.Playstyle}'"  
        if order |> Array.filter (fun x -> x = Int) |> Array.length <> 1 then failwith $"Could not find Intelligence Score in Ability Score order for class '{this.Class.Value.Name}, {this.Class.Value.Playstyle}'"  
        if order |> Array.filter (fun x -> x = Wis) |> Array.length <> 1 then failwith $"Could not find Wisom Score in Ability Score order for class '{this.Class.Value.Name}, {this.Class.Value.Playstyle}'"  
        if order |> Array.filter (fun x -> x = Cha) |> Array.length <> 1 then failwith $"Could not find Charisma Score in Ability Score order for class '{this.Class.Value.Name}, {this.Class.Value.Playstyle}'"  
        let sortedAbilityScores = abilityScores |> Array.sortDescending
        let rec iterateScores ind (char:Character) =
            if ind > 5 then 
                char
            else
                let nextAbilityScore = order.[ind]
                match nextAbilityScore with
                | Str   -> {char with Str = sortedAbilityScores.[ind]}
                | Dex   -> {char with Dex = sortedAbilityScores.[ind]}
                | Con   -> {char with Con = sortedAbilityScores.[ind]}
                | Wis   -> {char with Wis = sortedAbilityScores.[ind]}
                | Int   -> {char with Int = sortedAbilityScores.[ind]}
                | Cha   -> {char with Cha = sortedAbilityScores.[ind]}
                | Any   -> failwith "Error. No Character ability score array should contain the 'Any' value."
                |> iterateScores (ind+1)
        iterateScores 0 this

    member this.adjustByRace(race:Races.Race) =
        let rec adjustByAbilityScoreBonus(bonusMalus:Races.AbilityScoreBonus) (character:Character) =
            match bonusMalus.AbilityScore with
            | Str   -> {character with Str = character.Str + bonusMalus.Plus_Minus}
            | Dex   -> {character with Dex = character.Dex + bonusMalus.Plus_Minus}
            | Con   -> {character with Con = character.Con + bonusMalus.Plus_Minus}
            | Wis   -> {character with Wis = character.Wis + bonusMalus.Plus_Minus}
            | Int   -> {character with Int = character.Int + bonusMalus.Plus_Minus}
            | Cha   -> {character with Cha = character.Cha + bonusMalus.Plus_Minus}
            | Any   -> 
                if character.Class.IsNone then failwith "Error! Cannot use 'adjustByRace' before adding a class to the character."
                if bonusMalus.Plus_Minus > 0 then
                    character.Class.Value.Playstyle.AbilityScoreOrder.[0]
                elif bonusMalus.Plus_Minus < 0 then
                    character.Class.Value.Playstyle.AbilityScoreOrder.[5]
                else failwith $"AbilityScoreBonus/Malus through Race should never be 0: '{bonusMalus}'" 
                |> fun stat -> 
                    let anyToMainStatBonus = Races.AbilityScoreBonus.create(bonusMalus.Plus_Minus, stat)
                    adjustByAbilityScoreBonus anyToMainStatBonus character
        let mutable character = this
        race.AbilityScorePlus |> Array.iter (fun x -> 
            let nextCharacter = adjustByAbilityScoreBonus x character
            character <- nextCharacter
        )
        race.AbilityScoreMinus |> Array.iter (fun x ->
            let nextCharacter = adjustByAbilityScoreBonus x character
            character <- nextCharacter
        )
        character

open Races

let private adjustAbilityScoresByLevelIncreases(character:Character) =
        //!!// Helper
        let isOdd(n:int) = n%2 <> 0
        let isEven(n:int) = n%2 = 0
        /// This will be important for debugging(testing)
        let printIncrease(statToIncrease:AbilityScore, increaseN:int) =
            let level = increaseN*4
            printfn $"Increase {statToIncrease} at level {level}"
        let testSecondary (abilityScoreArr:(AbilityScore*int)[], oddScoresToFind:int) =
            let secondary = abilityScoreArr.[1..2] |> Array.filter (snd >> isOdd)
            let tertiary = abilityScoreArr.[3..] |> Array.filter (fun x -> snd x |> isOdd && snd x >= 13)
            let filteredScores = 
                [|
                    yield! secondary
                    yield! tertiary
                |] 
                |> Array.length
            filteredScores >= oddScoresToFind
        //!!// Helper end
        let nLevelIncreases = character.Level/4
        if character.Class.IsNone then failwith "Error. Character needs assigned class to use 'tryFindBestIncrease' to evaluate ability score increases!"
        let rec characterStatIncrease (character:Character) (currentIncrease:int) =
            let abilityScoreArray = character.Class.Value.Playstyle.AbilityScoreOrder |> Array.map (fun x -> x, character.getAbilityScore x)
            /// Highest stat has highest priority
            let mainStat = abilityScoreArray.[0]
            printfn "Show current character ability scores:"
            printfn $"Str {character.Str} Dex {character.Dex} Con {character.Con} Int {character.Int} Wis {character.Wis} Cha {character.Cha}"
            if currentIncrease > nLevelIncreases then 
                character
            else
                match character with
                /// If the characters main stat is odd, it is always good to increase
                | mainStatIsOdd when snd mainStat |> isOdd -> 
                    printfn "mainStatIsOdd"
                    printIncrease(fst mainStat, 1)
                    let inc = character.updateAbilityScore(fst mainStat, 1)
                    characterStatIncrease inc (currentIncrease+1)
                /// This will happen when you have a even main stat and there is only the level 20 increase left
                /// If there is no odd ability score for the last increase, increase the main stat
                | mainStatEven_OnlyLvl20Left when snd mainStat |> isEven && currentIncrease = 5 ->
                    printfn "mainStatEven_OnlyLvl20Left"
                    let firstUneven = abilityScoreArray |> Array.tryFind (fun x -> snd x |> isOdd)
                    let abilityScoreToIncrease = if firstUneven.IsNone then mainStat else firstUneven.Value
                    printIncrease(fst abilityScoreToIncrease, 1)
                    let inc = character.updateAbilityScore(fst abilityScoreToIncrease, 1)
                    characterStatIncrease inc (currentIncrease+1)
                // 18, 17, 17, x, x, x // 18, 17, 16, x, x, x
                /// This will happen with an even main stat, an odd leftover of increases and an odd secondary stat.
                /// By doing this the odd ability score will be added to the secondary stat and all even leftovers should go to main stat
                | mainStatEven_IncreasesLeftOdd_SecondaryOdd when snd mainStat |> isEven && isOdd (6-currentIncrease) && (testSecondary(abilityScoreArray,1)) ->
                    printfn "mainStatEven_IncreasesLeftOdd_SecondaryOdd"
                    let firstUneven = abilityScoreArray |> Array.find (fun x -> snd x |> isOdd)
                    printIncrease(fst firstUneven, 1)
                    let inc = character.updateAbilityScore(fst firstUneven, 1)
                    characterStatIncrease inc (currentIncrease+1)
                /// This will happen with an even main stat and both secondary stats odd. If this happens with even leftover increases this will 
                /// increase the first secondary and should trigger 'mainStatEven_IncreasesLeftOdd_SecondaryOdd' afterwards.
                | mainStatEven_IncreasesLeftEven_TwoSecondaryOdd when snd mainStat |> isEven && isEven (6-currentIncrease) && testSecondary(abilityScoreArray,2) ->
                    printfn "mainStatEven_IncreasesLeftEven_TwoSecondaryOdd"
                    let firstUneven = abilityScoreArray |> Array.find (fun x -> snd x |> isOdd)
                    printIncrease(fst firstUneven, 1)
                    let inc = character.updateAbilityScore(fst firstUneven, 1)
                    characterStatIncrease inc (currentIncrease+1)
                | default_mainStatEven -> 
                    printfn "default_mainStatEven"
                    printIncrease(fst mainStat, 1)
                    let inc = character.updateAbilityScore(fst mainStat, 1)
                    characterStatIncrease inc (currentIncrease+1)
        if nLevelIncreases > 0 then
            characterStatIncrease character 1
        else character    

let generateRandomCharacter (abilityGen:AbilityScoreGen) (level:int) (classCollection: (unit -> Class)[]) (races: Race [])= 
    let random = createNewRandom()
    let charClass = classCollection[random.Next(0,classCollection.Length)]
    let rndClass = charClass()
    /// Set level
    let setLevel level (char:Character) =
        {char with Level = level}
    /// Apply Ability Scores according to Class
    let applyAbilityScores (abilityGen:AbilityScoreGen) (char:Character) = 
        let abilityScoreArr = abilityGen.roller()
        printfn "AbilityScores unassigned: %A" abilityScoreArr
        char.applyAbilityScores abilityScoreArr
    /// Apply BAB according to class BAB progression and Character level
    let setBAB(char:Character) =
        if char.Class.IsNone then failwith $"Error. Character needs assigned class to use 'setBAB'!"
        {char with BAB = char.Class.Value.BABProgression.ofLevel char.Level}
    let setPseudoRandomRace (races: Race []) (character:Character) = 
        let filterOutDecreaseOnMainStats = 
            if character.Class.IsNone then failwith $"Error. Character needs assigned class to use 'classMainStats'!"
            let classMainStats = character.Class.Value.Playstyle.AbilityScoreOrder.[0..3] |> Set.ofArray
            races |> Array.filter (fun race ->
                let raceDumps = race.AbilityScoreMinus |> Array.map (fun x -> x.AbilityScore) |> Set.ofArray
                let isOverlapping = Set.intersect classMainStats raceDumps |> Set.count |> fun x -> x = 0 
                isOverlapping
            )
        let pseudoRandomRace = filterOutDecreaseOnMainStats.[random.Next(0,filterOutDecreaseOnMainStats.Length)]
        {character.adjustByRace pseudoRandomRace with Race = Some pseudoRandomRace}

    /// Init Character with Class
    let char = {Character.init with Class = Some rndClass}
    char
    |> setLevel level
    |> applyAbilityScores abilityGen
    |> setBAB
    |> setPseudoRandomRace races
    |> adjustAbilityScoresByLevelIncreases

let Harald = generateRandomCharacter AbilityScoreGen.Heroic 5 (CharacterClasses.Core.balancedCollection()) Races.Core.collection
