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

-- Function to create software instances
createSoftware :: String -> Int -> Software
createSoftware name size = Software name size

-- Function to create a container with a list of software
createContainer :: [Software] -> Container
createContainer softwareList = Container softwareList False

-- Function to create a compose with a list of containers
createCompose :: [Container] -> Compose
createCompose containers = Compose containers

-- Function to add software to a container
addSoftwareToContainer :: Software -> Container -> Container
addSoftwareToContainer software container =
  container { containerSoftware = software : containerSoftware container }

-- Function to add a container to compose
addContainerToCompose :: Container -> Compose -> Compose
addContainerToCompose container compose =
  compose { composeContainers = container : composeContainers compose }

-- Function to run all containers in compose
runCompose :: Compose -> Compose
runCompose compose =
  compose { composeContainers = map runContainer (composeContainers compose) }
  where
    runContainer container = container { containerStatus = True }

-- Function to stop a container
stopContainer :: Container -> Container
stopContainer container = container { containerStatus = False }

-- Function to get the real-time size of containers with active software
getTamanhoTempoReal :: Compose -> Int
getTamanhoTempoReal compose =
  sum [softwareSize s | container <- composeContainers compose, containerStatus container, s <- containerSoftware container]

-- Example usage
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