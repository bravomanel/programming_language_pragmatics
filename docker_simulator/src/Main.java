public class Main {

    public static void main(String[] args) {
        Compose compose = new Compose();
        System.out.println("Compose created: " + compose.toString());
        System.out.println("Build the containers and add some softwares: ");
        Menu menu = new Menu(compose);
        while (true) {
            menu.mainMenu();
        }
    }
    
}