import java.util.*;

public class Container {
    private final List<Software> container;
    private String name;
    private boolean status;
    private int containerSize = 0;

    public Container(String name) {
        container = new ArrayList<Software>();
        this.name = name;
        this.status = false;
    }

    public String getName() {
        return this.name;
    }

    public boolean getStatus() {
        return this.status;
    }
    
    public int getSize() {
        return this.containerSize;
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
            containerSize++;
        } else {
            System.out.println("Container is running, please stop the execution to add a software \n");
        }
    }

    public void removeSoftware(Software software) {
        if (!this.status) {
            if (container.contains(software)) {
                container.remove(software);
                containerSize--;
            } else {
                System.out.println("Container don't have this software \n");
            }
        } else {
            System.out.println("Container is running, please stop the execution to remove a software \n");
        }
    }

    public Software getSoftware(int id) {
        if (id <= containerSize) {
            return container.get(id-1);
        } else {
            System.out.println("Invalid Software ID");
            return null;
        }
    }

    public String listSoftware() {
        String str = "Container total softwares: " + containerSize + "\n";
        int i = 1;
        for (Software software : container) {
            str = str + i + " - " + software.getName() + "\n";
            i++;
        }
        return str;
    }

    public String toString(){
        String status = "Status: " + (this.status ? "Running" : "Stopped") + "\n";
        String totalSize = "Container size: " + getSize() + "\n";
        String softwareSize = "This container have " + container.size() + " software's \n";
        for(Software software: container)
            softwareSize += "- " + software.toString() + "\n";

        status = status + totalSize + softwareSize;

        return status;
    }
    
}