public class Menu {

    Compose compose;

    public Menu(Compose compose) {
        this.compose = compose;
    }

    public void mainMenu() {
        // L.O.O.B.O.S Language Oriented Object Bazilio Operating System
        System.out.println("\n LOOBOS -- Whale Simulator -- \n");
        System.out.println("1 - Compose");
        System.out.println("2 - Container");
        System.out.println("3 - Software");
        System.out.println("4 - Status");
        System.out.println("0 - Exit");
        System.out.print("Choose an option: ");
        int option = Integer.parseInt(System.console().readLine());

        switch (option) {
            case 1:
                composeMenu();
                break;
            case 2:
                containerMenu();
                break;
            case 3:
                softwareMenu();
                break;
            case 4:
                statusMenu();
                break;
            case 0:
                System.exit(0);
                break;
            default:
                System.out.println("Invalid option!");
                break;
        }
    }

    private void composeMenu() {
        System.out.println("1 - Run Compose");
        System.out.println("2 - Stop Compose");
        System.out.println("3 - Status Compose");
        System.out.println("0 - Go back");
        System.out.print("Choose an option: ");

        int option = Integer.parseInt(System.console().readLine());
        switch(option) {
            case 1:
                runCompose();
                break;
            case 2:
                stopCompose();
                break;
            case 3:
                statusCompose();
                break;
            case 0:
                mainMenu();
                break;
            default:
                System.out.println("Invalid option!");
                break;
        }
    }

    private void containerMenu () {
        System.out.println("1 - Create a Container");
        System.out.println("2 - Destroy Container");
        System.out.println("3 - Run a Container");
        System.out.println("4 - Stop a Container");
        System.out.println("5 - Status Container");
        System.out.println("0 - Go back");
        System.out.print("Choose an option: ");

        int option = Integer.parseInt(System.console().readLine());
        switch(option) {
            case 1:
                createContainer();
                break;
            case 2:
                removeContainer();
                break;
            case 3:
                runContainer();
                break;
            case 4:
                stopContainer();
                break;
            case 0:
                mainMenu();
                break;
            default:
                System.out.println("Invalid option!");
                break;
        }

    }

    private void softwareMenu() {
        System.out.println("1 - Create a Software");
        System.out.println("2 - Destroy Software");
        System.out.println("3 - List Software");
        System.out.println("0 - Go back");
        System.out.print("Choose an option: ");

        int option = Integer.parseInt(System.console().readLine());
        switch(option) {
            case 1:
                createSoftware();
                break;
            case 2:
                removeSoftware();
                break;
            case 3:
                statusSoftware();
                break;
            case 0:
                mainMenu();
                break;
            default:
                System.out.println("Invalid option!");
                break;
        }
    }

    private void statusMenu() {
        System.out.println("1 - Compose Status");
        System.out.println("2 - Container Status");
        System.out.println("3 - Software Status");
        System.out.println("0 - Go back");
        System.out.print("Choose an option: ");

        int option = Integer.parseInt(System.console().readLine());
        switch(option) {
            case 1:
                statusCompose();
                break;
            case 2:
                statusContainer();
                break;
            case 3:
                statusSoftware();
                break;
            case 0:
                mainMenu();
                break;
            default:
                System.out.println("Invalid option!");
                break;
        }
    }

    private Container chooseContainer() {
        System.out.print(compose.toString());
        if (compose.getSize() == 0) {
            System.out.println("No containers created");
            return null;
        } else {
            System.out.print("Container ID: ");
            int containerId = Integer.parseInt(System.console().readLine());
            Container container = compose.getContainer(containerId);
            return container;
        }
    }

    private Software chooseSoftware() {
        Container container = chooseContainer();
        if (container == null) {
            return null;
        } else {
            System.out.print(container.listSoftware());
            if (container.getSize() == 0) {
                System.out.println("No softwares created");
                return null;
            } else {
                System.out.print("Software ID: ");
                int softwareId = Integer.parseInt(System.console().readLine());
                Software software = container.getSoftware(softwareId);
                return software;
            }
        }
    }

    private void createContainer() {
        if (compose.getStatus()) {
            System.out.println("Compose is running, please stop the execution to add a container \n");
        } else {
            System.out.print("Container name: ");
            String name = System.console().readLine();
            Container container = new Container(name);
            compose.addContainer(container);
            System.out.println("Container created: " + container.toString());
        }
         
    }

    private void removeContainer() {
        if (compose.getStatus()) {
            System.out.println("Compose is running, please stop the execution to destroy it \n");
        } else {
            System.out.print(compose.toString());
            System.out.print("Container ID: ");
            int containerId = Integer.parseInt(System.console().readLine());
            Container container = compose.getContainer(containerId);
            compose.removeContainer(container);
        }
    }

    private void createSoftware() {
        if (compose.getStatus()) {
            System.out.println("Compose is running, please stop the execution \n");
        } else {
            Container container = chooseContainer();        
            if (container != null) {
                System.out.print("Software name: ");
                String name = System.console().readLine();
                System.out.print("Software size: ");
                int size = Integer.parseInt(System.console().readLine());
                Software software = new Software(name, size);
                container.addSoftware(software);
                System.out.println("Software created: " + software.toString());
            }
        }
    }

    private void removeSoftware() {
        Container container = chooseContainer();
        if (container != null) {
            Software software = chooseSoftware();
            container.removeSoftware(software);
        }
    }

    private void runCompose() {
        compose.run();
    }

    private void stopCompose() {
        compose.stop();
    }

    private void runContainer() {
        Container container = chooseContainer();
        container.run();
    }

    private void stopContainer() {
        Container container = chooseContainer();
        container.stop();
    }

    private void statusCompose() {
        System.out.println(compose.toString());
    }

    private void statusContainer() {
        Container container = chooseContainer();
        if (container != null) {
            System.out.println(container.listSoftware());
        }
    }

    private void statusSoftware() {
        Software software = chooseSoftware();
        if (software != null) {
            System.out.println(software.toString());
        }
    }

}