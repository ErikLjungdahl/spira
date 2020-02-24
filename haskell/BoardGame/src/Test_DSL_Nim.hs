import Ceptre_DSL

main :: IO ()
main = do
    let sticks = createCountSticks ["z","s nat"]
    write sticks "Ceptre.cep"
    let nbrOfSticks = createNbrOfSticks ["sticks nat"]
    write nbrOfSticks "Ceptre.cep"
    let player = createPlayer ["name1","name2"]
    write player "Ceptre.cep"
    let oppPlayer = createOppPlayer "name1" "name2"
    write oppPlayer "Ceptre.cep"
    let pred = predicate ["win","turn"]
    write pred "Ceptre.cep"
    let stage = createStage "rules" ["takeOne","takeTwo","takeThree"]
    write stage "Ceptre.cep"

    