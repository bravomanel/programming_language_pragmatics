import java.util.*;

public class Container {
    private final List<Software> container;
    private boolean status;

    public Container() {
        container = new ArrayList<Software>();
        this.status = false;
    }

    public boolean getStatus() {
        return this.status;
    }

    public String toString(){
        String size = "this container have " + container.size() + " software's \n";
        for(Software software: container) {
            size = size + software.toString() + "\n";
        }
        return size;
    }

    public void addSoftware(Software software) {
        if (!this.status) {
            container.add(software);
        } else {
            System.out.println("Container is running, please stop the execution to add a software \n");
        }
    }

    public int getSize() {
        int size = 0;
        for (Software object: container) {
            size = size + object.size;
        }
        return size;
    }

    public void run() {
        this.status = true;
    }

    public void stop() {
        this.status = false;
    }

}
