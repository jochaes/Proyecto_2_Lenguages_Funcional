
open System
// let rec deleteString (str: string) (lst: string list) =
//   match lst with
//   | [] -> []
//   | h::t when (Seq.sort h) = (Seq.sort str) -> t
//   | h::t -> h :: deleteString str t



// let myList = ["apple"; "banana"; "cherry"; "date"]
// let updatedList = deleteString "banaan" myList
// printfn "%A" updatedList // Output: ["apple"; "cherry"; "date"]
  
//printfn "%b" ( (Seq.sort "banana").ToString() = (Seq.sort "banaan").ToString() )

let deleteString (strList : string list) (strToRemove : string) =
  let removeStr = String.Concat( List.sort ( strToRemove.ToCharArray() |> List.ofArray ) )
  strList |> List.filter (fun (s:string) -> String.Concat( List.sort ( s.ToCharArray() |> List.ofArray ) ) <> removeStr)
  
  
let myList = ["apple"; "banana"; "cherry"; "date"]
let updatedList = deleteString myList "abaann" 
printfn "%A" updatedList // Output: ["apple"; "cherry"; "date"]