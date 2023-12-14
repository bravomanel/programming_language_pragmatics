public class Main {
    public static void main(String[] args) {

        Software vscode = new Software("VScode", 200);
        Software ubuntu = new Software("Ubuntu", 2000);
        Software chrome = new Software("Chrome", 2500);
        Software minecraft = new Software("Minecraft", 500);

        System.out.println("Software: " + vscode.name + " Size: " + vscode.size);
        System.out.println("Software: " + minecraft.name + " Size: " + minecraft.size);

        //create a instance of container and add softwares
        Container emanuelBravo = new Container();
        //add 2 software's
        emanuelBravo.addSoftware(ubuntu);
        emanuelBravo.addSoftware(minecraft);
        //container run
        emanuelBravo.run();
        System.out.println("Status container ebravo: " + (emanuelBravo.getStatus() ? "Running" : "Stopped") + "\n");
        //error
        emanuelBravo.addSoftware(chrome);

        System.out.println("emanuelBravo length: \n " + emanuelBravo.toString());

        //create a instance of container
        Container arthurPeixoto = new Container();
        arthurPeixoto.addSoftware(minecraft);

        System.out.println(arthurPeixoto.toString());

        Compose build = new Compose();
        build.addContainer(emanuelBravo);
        build.addContainer(arthurPeixoto);

        build.run();

        System.out.println("Size of containers: " + build.sizeComposeRunning() + "\n");

        build.stop();

        System.out.println("Size of containers: " + build.sizeComposeRunning() + "\n");

    }
}
