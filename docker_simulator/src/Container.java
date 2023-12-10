import java.util.*;

public class Container {
    private final List<Software> container;
    private boolean status;

    public Container() {
        container = new ArrayList<Software>();
        this.status = false;
    }

    public boolean statusContainer() {
        return this.status;
    }

    public void run() {
        this.status = true;
    }

    public void stop() {
        this.status = false;
    }

    public void addSoftware(Software software) {
        if (!this.status) {
            container.add(software);
        } else {
            System.out.println("Container is running, please stop the execution to add a software \n");
        }
    }

    public boolean hasSoftware(Software software) {
        if (container.contains(software)) {
            // System.out.println("Container have this software \n");
            return true;
        } else {
            // System.out.println("Container don't have this software \n");
            return false;
        }
    }
    
    public void removeSoftware(Software software) {
        if (!this.status) {
            if (container.contains(software)) {
                container.remove(software);
            } else {
                System.out.println("Container don't have this software \n");
            }
        } else {
            System.out.println("Container is running, please stop the execution to remove a software \n");
        }
    }

    public int sizeContainer() {
        int size = 0;
        for (Software object: container)
            size = size + object.size;

        return size;
    }

    public String toString(){
        String status = "Status: " + (this.status ? "Running" : "Stopped") + "\n";
        String totalSize = "Container size: " + sizeContainer() + "\n";
        String softwareSize = "This container have " + container.size() + " software's \n";
        for(Software software: container)
            softwareSize += "- " + software.toString() + "\n";

        status = status + totalSize + softwareSize;

        return status;
    }
    
}