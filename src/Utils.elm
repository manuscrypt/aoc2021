module Utils exposing (..)

import Posix.IO as IO exposing (IO)
import Posix.IO.File as File
import Posix.IO.Process as Proc


path : String -> String -> String
path dir file =
    [ "C:/", "src", "aoc2021", "data", dir, file ]
        |> String.join "/"


getInput : String -> String -> IO (List String)
getInput dir file =
    path dir file
        |> readAllLines


readAllLines : String -> IO (List String)
readAllLines filename =
    IO.do
        (File.contentsOf filename
            |> IO.exitOnError identity
        )
        (String.lines
            >> IO.return
        )


output : String -> Int -> IO ()
output prefix num =
    IO.do (Proc.print (prefix ++ String.fromInt num))
        return


return : a -> IO ()
return _ =
    IO.return ()
