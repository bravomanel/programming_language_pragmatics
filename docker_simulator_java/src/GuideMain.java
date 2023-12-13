public class GuideMain {

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
        Container dbContainer = new Container();
        Container staticContainer = new Container();

        //adding software's to each container
        webContainer.addSoftware(tomcat);
        webContainer.addSoftware(java);
        webContainer.addSoftware(debian);
        dbContainer.addSoftware(sqlServer);
        dbContainer.addSoftware(dotNet);
        dbContainer.addSoftware(ubuntu);
        staticContainer.addSoftware(staticBinary);
        staticContainer.addSoftware(alpine);

        //container run
        System.out.println("\nRunning Containers!\n");
        webContainer.run();
        dbContainer.run();
        staticContainer.run();

        //error
        System.out.println("\nExample of error: trying to add a software while container is running\n");
        webContainer.addSoftware(ubuntu);
        
        //print the containers
        System.out.println("\nwebContainer:\n" + webContainer.toString());
        System.out.println("\ndbContainer:\n" + dbContainer.toString());
        System.out.println("\nstaticContainer:\n" + staticContainer.toString());

        //creating and running a instance of compose
        Compose build = new Compose();
        build.addContainer(webContainer);
        build.addContainer(dbContainer);
        build.addContainer(staticContainer);

        build.run();
        System.out.println("Containers running: " + build.getSize() + "\n");

        System.out.println("Stopping Compose!\n");
        build.stop();
        
        System.out.println("Containers running: " + build.getSize() + "\n");

    }

}