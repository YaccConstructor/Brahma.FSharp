module ArrayGPU.Tests
open NUnit.Framework

[<Test>]
let ``Map test``() =
  let arr = [|for i in 1..1000000 -> i|]
  let t = ArrayGPU.init arr
  let outarr = ArrayGPU.getResult (ArrayGPU.Map (<@ fun x -> x + 2 @>) arr t) t
  Assert.AreEqual ([|for i in 1..1000000 -> i + 2|], outarr)

[<Test>]
let ``Mapi test``() =
  let arr = [|for i in 1..10 -> i|]
  let t = ArrayGPU.init arr
  let outarr = ArrayGPU.getResult (ArrayGPU.Mapi (<@ fun x i -> x + i @>) arr t) t
  Assert.AreEqual ([|1; 3; 5; 7; 9; 11; 13; 15; 17; 19|], outarr)

[<Test>]
let ``Map2 test``() =
  let arr1 = [|for i in 1..10 -> i|]
  let arr2 = [|for i in 1..10 -> i|]
  let t = ArrayGPU.init arr1
  let outarr =  ArrayGPU.getResult (ArrayGPU.Map2 (<@ fun x i -> x + i @>) arr1 arr2 t) t
  Assert.AreEqual ([|for i in 1..10 -> 2 * i|], outarr)

[<Test>]
let ``Reverse test``() =
  let arr = [|for i in 1..10 -> i|]
  let t = ArrayGPU.init arr
  let outarr = ArrayGPU.getResult (ArrayGPU.Reverse arr t) t
  Assert.AreEqual ([|10; 9; 8; 7; 6; 5; 4; 3; 2; 1|], outarr)


[<Test>]
let ``Reverse (Map) test``() =
  let arr = [|for i in 1..10 -> i|]
  let t = ArrayGPU.init arr
  let outarr = ArrayGPU.getResult (ArrayGPU.Reverse (ArrayGPU.Map (<@ fun x -> x + 2 @>) arr t) t) t
  Assert.AreEqual ([|12; 11; 10; 9; 8; 7; 6; 5; 4; 3|], outarr)

[<Test>]
let ``Mapi (Map2) test``() =
  let arr1 = [|for i in 1..5 -> i|]
  let arr2 = [|for i in 1..5 -> i|]
  let t = ArrayGPU.init arr1
  let outarr = ArrayGPU.getResult (ArrayGPU.Mapi (<@ fun x i -> x * i @>) (ArrayGPU.Map2 (<@ fun x1 x2 -> x1 - x2 @>) arr1 arr2 t) t) t
  Assert.AreEqual ([|0; 0; 0; 0; 0|], outarr)

[<Test>]
let ``Exception test 1``() =
  let arr = [||]
  Assert.That((fun _ -> ArrayGPU.init arr), Throws.Exception) 

[<Test>]
let ``Exception test 2``() =
  let arr1 = [|1; 2; 3; 4; 5|]
  let arr2 = [|1; 2; 3; 4; 5; 6; 7|]
  let t = ArrayGPU.init arr1
  Assert.That((fun _ -> ArrayGPU.Map2 (<@ fun x1 x2 -> x1 - x2 @>) arr1 arr2 t), Throws.Exception)

