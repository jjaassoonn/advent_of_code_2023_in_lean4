import Std.Data.HashMap

open Std

namespace day5.part1

--  List Nat × List (List String)
def processContent (s : String) : List Nat × List (List (Nat × Nat × Nat)) :=
  let s' : List String := s.splitOn "\n\n"
    |>.map (String.splitOn · ":")
    |>.map (String.trim <$> ·)
    |>.map (List.get! · 1)
  match s' with
  | seed :: data =>
    let data' : List <| List (Nat × Nat × Nat) := data
      |>.map (String.splitOn · "\n")
      |>.map ((String.splitOn · " ") <$> ·)
      |>.map ((String.toNat! <$> ·) <$> ·)
      |>.map ((fun | [a, b, c] => (a, b, c) |_ => panic! "line 19") <$> ·)
    ( seed.splitOn " " |>.map String.toNat!, data')
  | [] => panic! "line 47: shouldn't happen, empty list"

def getSeed (s : String) : List Nat :=
  (processContent s).1

def seed2SoilMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 0
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def soil2FertilizerMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 1
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def fertilizer2waterMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 2
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def water2lightMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 3
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def light2temperatureMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 4
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def temperature2humidityMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 5
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def humidity2locationMap (s : String) : Nat → Nat := Id.run do
  let dat : List (Nat × Nat × Nat) := (processContent s).2.get! 6
  let mut res : Nat → Nat := id
  for (desStart, sourceStart, range) in dat do
    res := fun n ↦
      if sourceStart ≤ n && n < sourceStart + range
      then desStart + (n - sourceStart)
      else res n
  return res

def getSoil (s : String) (seed : Nat) : Nat :=
  seed2SoilMap s seed

def getFertilizer (s : String) (seed : Nat) : Nat :=
  let soil := getSoil s seed
  soil2FertilizerMap s soil

def getWater (s : String) (seed : Nat) :=
  let fertilizer := getFertilizer s seed
  fertilizer2waterMap s fertilizer

def getLight (s : String) (seed : Nat) :=
  let water := getWater s seed
  water2lightMap s water

def getTemperature (s : String) (seed : Nat) :=
  let light := getLight s seed
  light2temperatureMap s light

def getHumidity (s : String) (seed : Nat) :=
  let temperature := getTemperature s seed
  temperature2humidityMap s temperature

def getLocation  (s : String) (seed : Nat) :=
  let humidity := getHumidity s seed
  humidity2locationMap s humidity

def analyseData (s : String) : List Nat :=
  getSeed s |>.map (getLocation s)

def List.min (l : List Nat) : Nat :=
match l with
| [] => 0
| [x] => x
| x::xs => x.min (List.min xs)

def exampleString :=
s!"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

#eval analyseData exampleString

def main : IO Unit:= do
  let content : String ← IO.FS.readFile "Advent/day5/data.txt"
  IO.println <| List.min <| analyseData content

end day5.part1
