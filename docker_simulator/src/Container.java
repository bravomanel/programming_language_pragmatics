import java.util.*;

public class Container {
    private final List<Software> container;
    private boolean status;

    public Container() {
        container = new ArrayList<Software>();
        this.status = false;
    }

    // function for return status of container ir running or not
    public boolean statusContainer() {
        return this.status;
    }

    public String toString(){
        String size = "this container have " + container.size() + " software's \n";

        for(Software software: container)
            size = size + software.toString() + "\n";

        return size;
    }

    public void addSoftware(Software software) {
        // verify if container is running
        if (!this.status) {
            container.add(software);
        }
        else{
        System.out.println("Container is running, please stop de execution for add softwares \n");
        }
    }

    public int sizeContainer() {
        int size = 0;
        for (Software object: container)
            size = size + object.size;

        return size;
    }

    public void run() {
        this.status = true;
    }

    public void stop() {
        this.status = false;
    }

}


