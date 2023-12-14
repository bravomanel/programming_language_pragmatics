data Software = Software
  { softwareName :: String
  , softwareSize :: Int
  } deriving (Show)

data Container = Container
  { containerSoftware :: [Software]
  , containerStatus :: Bool
  } deriving (Show)

data Compose = Compose
  { composeContainers :: [Container]
  } deriving (Show)

createSoftware :: String -> Int -> Software
createSoftware name size = Software name size

createContainer :: [Software] -> Container
createContainer softwareList = Container softwareList False

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

      web = createContainer [tomcat, java, debian]
      db = createContainer [sqlServer, dotNET, ubuntu]
      static = createContainer [staticBinary, alpine]

      compose = createCompose [web, db, static]

  let updatedCompose = runCompose compose
      stoppedContainer = stopContainer (head (composeContainers updatedCompose))

  print updatedCompose
  print stoppedContainer

  let totalSize = getTamanhoTempoReal updatedCompose
  putStrLn $ "Total Size: " ++ show totalSize