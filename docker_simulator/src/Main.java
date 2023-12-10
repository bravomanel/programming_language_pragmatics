public class Main {
    public static void main(String[] args) {
        //creating the software's
        Software tomcat = new Software("Tomcat", 300);
        Software java = new Software("Java", 1000);
        Software debian = new Software("Debian", 2000);
        Software sqlServer = new Software("SQL Server", 400);
        Software dotNet = new Software(".NET", 1000);
        Software ubuntu = new Software("Ubuntu", 2500);
        Software staticBinary = new Software("Static Binary", 200);
        Software alpine = new Software("Alpine", 800);


        //creating the containers
        Container webContainer = new Container();
        Container sqlContainer = new Container();
        Container staticContainer = new Container();

        //adding software's to each container
        webContainer.addSoftware(tomcat);
        webContainer.addSoftware(java);
        webContainer.addSoftware(debian);
        sqlContainer.addSoftware(sqlServer);
        sqlContainer.addSoftware(dotNet);
        sqlContainer.addSoftware(ubuntu);
        staticContainer.addSoftware(staticBinary);
        staticContainer.addSoftware(alpine);

        //container run
        System.out.println("\nRunning Containers!\n");
        webContainer.run();
        sqlContainer.run();
        staticContainer.run();

        //error
        System.out.println("\nExample of ERROR: trying to add a software while container is running\n");
        webContainer.addSoftware(ubuntu);
        
        System.out.println("\nwebContainer:\n" + webContainer.toString());
        System.out.println("\nsqlContainer:\n" + sqlContainer.toString());
        System.out.println("\nstaticContainer:\n" + staticContainer.toString());

        Compose build = new Compose();
        build.addContainer(webContainer);
        build.addContainer(sqlContainer);
        build.addContainer(staticContainer);

        build.run();
        System.out.println("Size of Compose: " + build.sizeCompose() + "\n");

        System.out.println("Stopping Compose!\n");
        build.stop();
        System.out.println("Size of Compose: " + build.sizeCompose() + "\n");

    }
    
}