let generateWordSearch(words: string list) =
    let gridSize = 10
    let rand = System.Random()
    let directionVectors = [(0, 1); (1, 0); (1, 1); (0, -1); (-1, 0); (-1, -1); (-1, 1); (1, -1)]
    let matrix = Array2D.create gridSize gridSize ' '

    let rec tryPlaceWord(word: string) =
        let x = rand.Next(0, gridSize)
        let y = rand.Next(0, gridSize)
        let direction = directionVectors.[rand.Next(0, directionVectors.Length)]
        if x + fst(direction) * (word.Length-1) >= gridSize || y + snd(direction) * (word.Length-1) >= gridSize ||
           x + fst(direction) * (word.Length-1) < 0 || y + snd(direction) * (word.Length-1) < 0 then
            tryPlaceWord(word)
        else
            let mutable success = true
            for i in 0..(word.Length-1) do
                if matrix.[x + fst(direction)*i, y + snd(direction)*i] <> ' ' && matrix.[x + fst(direction)*i, y + snd(direction)*i] <> word.[i] then
                    success <- false
            if success then
                for i in 0..(word.Length-1) do
                    matrix.[x + fst(direction)*i, y + snd(direction)*i] <- word.[i]
            else
                tryPlaceWord(word)

    for word in words do
        tryPlaceWord(word)

    for i in 0..(gridSize-1) do
        for j in 0..(gridSize-1) do
            if matrix.[i, j] = ' ' then
                matrix.[i, j] <- (char)(rand.Next(65, 91))
                
    matrix


let words = ["ALMA"; "PATIO"; "CASA";"TELEFONO"]
let matrix = generateWordSearch words
printfn "%A" matrix

