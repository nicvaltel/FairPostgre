{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Example where


import System.IO
import Control.Monad.Reader
import Control.Exception
import  Data.ByteString.Char8 qualified as BC
import Data.ByteString qualified as B

import Lib


import Domain.Messenger
import Logger
import Data.Foldable (traverse_)


defaultDBConnInfo :: DBConnectionInfo
defaultDBConnInfo = 
  let username_ = "postgres"
      password_ = "123456"
      dbName_ = "products"
  in DBConnectionInfo
    { hostName = "127.0.0.1"
    , port = "5432"
    , dbName = dbName_
    , startupMsg = startupMessage username_ dbName_
    , username = "postgres"
    , password = password_,
    logParams = undefined
  }



-- |Startup message params. Do not change it without need.
startupMessage :: B.ByteString -> B.ByteString -> SendMessage
startupMessage uname database = StartupMessage  [("user", uname)
                                , ("database", database)
                                , ("client_encoding", "UTF8")
                                , ("standard_conforming_strings", "on")
                                , ("bytea_output", "hex")
                                , ("DateStyle", "ISO, YMD")
                                , ("IntervalStyle", "iso_8601")
                                , ("extra_float_digits", "3") ]



-- |Parameters of logging. PrintToConsole, SaveToFile FilePath, PrintAndSaveToFile FilePath and TurnOff possible options
defaultLogParams :: LogParams
defaultLogParams = LogParams 
  { paramSaveResultTo         = PrintAndSaveToFile "./out_log.log"
  , paramLoggerConnector      = PrintAndSaveToFile "./out_log.log"
  , paramLoggerSender         = PrintAndSaveToFile "./out_log.log"
  , paramLoggerReciever       = PrintAndSaveToFile "./out_log.log"
  }

-- |Main routine
routine :: App ()
routine = do
  conn  <- connect
  let run = execQuery conn 
  run "SELECT * FROM products" 
  run queryCreateSchema
  run queryCreatePersonTable
  run queryCreateQTimesTable
  traverse_ run queryInsertPersons
  


runExample :: IO ()
runExample = runApp defaultDBConnInfo routine




queryCreateSchema :: QueryStr
queryCreateSchema = "CREATE SCHEMA IF NOT EXISTS test_fair;"

queryCreatePersonTable :: QueryStr
queryCreatePersonTable = "CREATE TABLE IF NOT EXISTS test_fair.Person ( person_id INT NOT NULL, name VARCHAR ( 50 ) NOT NULL, AGE INT, kc_quantity INT NOT NULL, pants_color VARCHAR ( 50 ));"


queryCreateQTimesTable :: QueryStr
queryCreateQTimesTable = "CREATE TABLE IF NOT EXISTS test_fair.QTimes ( person_id INT NOT NULL, q_times INT );"

queryInsertPersons :: [QueryStr]
queryInsertPersons = [ "INSERT INTO test_fair.person (person_id, name, age, kc_quantity) VALUES (1, 'John', 30, 0);"
                     , "INSERT INTO test_fair.person (person_id, name, age, kc_quantity) VALUES (2, 'Kate', 22, 0);"
                     , "INSERT INTO test_fair.person (person_id, name, age, kc_quantity) VALUES (3, 'Bob', 27, 1);"
                     , "INSERT INTO test_fair.person (person_id, name, age, kc_quantity) VALUES (4, 'Mike', 55, 2);"
                     , "INSERT INTO test_fair.person (person_id, name, age, kc_quantity) VALUES (5, 'Alfonso', 66, 100);"]

