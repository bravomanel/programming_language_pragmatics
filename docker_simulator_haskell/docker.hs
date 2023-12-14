data Software = Software
  { softwareName :: String
  , softwareSize :: Int
  } deriving (Show)

data Container = Container
  { containerSoftware :: [Software]
  , containerName :: String
  , containerStatus :: Bool
  } deriving (Show)

data Compose = Compose
  { composeContainers :: [Container]
  } deriving (Show)

createSoftware :: String -> Int -> Software
createSoftware name size = Software name size

createContainer :: [Software] -> String -> Container
createContainer softwareList name = Container softwareList name False

createCompose :: [Container] -> Compose
createCompose containers = Compose containers

addSoftwareToContainer :: Software -> Container -> Container
addSoftwareToContainer software container =
  container { containerSoftware = software : containerSoftware container }

addContainerToCompose :: Container -> Compose -> Compose
addContainerToCompose container compose =
  compose { composeContainers = container : composeContainers compose }

runCompose :: Compose -> Compose
runCompose compose =
  compose { composeContainers = map runContainer (composeContainers compose) }
  where
    runContainer container = container { containerStatus = True }

stopCompose :: Compose -> Compose
stopCompose compose =
  compose { composeContainers = map stopContainer (composeContainers compose) }

stopContainer :: Container -> Container
stopContainer container = container { containerStatus = False }

getTamanhoTempoReal :: Compose -> Int
getTamanhoTempoReal compose =
  sum [softwareSize s | container <- composeContainers compose, containerStatus container, s <- containerSoftware container]

main :: IO ()
main = do
  let tomcat = createSoftware "Tomcat" 300
      java = createSoftware "Java" 1000
      debian = createSoftware "Debian" 2000
      sqlServer = createSoftware "SQL Server" 400
      dotNET = createSoftware ".NET" 1000
      ubuntu = createSoftware "Ubuntu" 2500
      staticBinary = createSoftware "Static Binary" 200
      alpine = createSoftware "Alpine" 800

      web = createContainer [tomcat, java, debian] "web"
      db = createContainer [sqlServer, dotNET, ubuntu] "db"
      static = createContainer [staticBinary, alpine] "static"

      compose = createCompose [web, db, static]

  let updatedCompose = runCompose compose
      stoppedCompose = stopCompose compose

  print java
  putStrLn "\n" 
  print web
  putStrLn "\n" 
  print updatedCompose
  putStrLn "\n" 
  print stoppedCompose
  putStrLn "\n" 
  

  let totalSizeRunning = getTamanhoTempoReal updatedCompose
  let totalSizeStopped = getTamanhoTempoReal stoppedCompose
  putStrLn $ "Total Size Running Compose: " ++ show totalSizeRunning
  putStrLn $ "Total Size Stopped Compose: " ++ show totalSizeStopped