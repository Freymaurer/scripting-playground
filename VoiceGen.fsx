#r "nuget: System.Speech, 5.0.0"

open System.IO
open System.Speech
open System.Speech.Synthesis
open System.Text

let output = 
    let synth = new SpeechSynthesizer()
    // let memoryStream = new MemoryStream()
    // synth.SetOutputToWaveStream(memoryStream)
    synth.SetOutputToWaveFile(@"C:\Users\Kevin\Desktop\test.wav")
    synth.Rate <- -5
    synth.Speak("Captial I.  5.  Captial E.  f.  Captial D.")
    ()
