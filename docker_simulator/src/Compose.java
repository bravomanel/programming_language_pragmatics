import java.util.*;

public class Compose {
    List<Container> compose;
    private int composeSize = 0;
    private boolean status = false;

    public Compose() {
        compose = new ArrayList<Container>();
    }

    public int getSize() {
        return this.composeSize;
    }

    public boolean getStatus() {
        return this.status;
    }

    public void addContainer(Container container) {
        if (status) {
            System.out.println("Compose is running, please stop the execution to add a container \n");
        } else {
            compose.add(container);
            composeSize++;
        }
    }

    public void removeContainer(Container container) {
        if (!container.getStatus()) {
            if (compose.contains(container)) {
                compose.remove(container);
                composeSize--;
            } else {
                System.out.println("Compose don't have this container \n");
            }
        } else {
            System.out.println("Compose is running, please stop the execution to destroy it \n");
        }
    }
    
    public Container getContainer(int id) {
        if (id <= composeSize) {
            return compose.get(id-1);
        } else {
            System.out.println("Invalid container ID");
            return null;
        }
    }

    public void run() {
        for (Container container : compose) {
            container.run();
        }
        status = true;
    }

    public void stop() {
        for (Container container : compose) {
            container.stop();
        }
        status = false;
    }

    public String toString() {
        String str = "Compose " + (status ? "" : "not ") + "running! \nTotal containers: " + composeSize + "\n";
        int i = 1;
        for (Container container : compose) {
            str = str + i + " - " + container.getName() + "\n";
            i++;
        }
        return str;
    }
    
}