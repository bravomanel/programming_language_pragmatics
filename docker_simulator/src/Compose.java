import java.util.*;

public class Compose {
    List<Container> compose;

    public Compose() {
        compose = new ArrayList<Container>();
    }

    public void addContainer(Container container) {
        compose.add(container);
    }

    public int sizeCompose() {
        int size = 0;
        for (Container container : compose) {
            if (container.statusContainer())
                size = size + container.sizeContainer();
        }
        return size;
    }

    public void run() {
        for (Container container : compose) {
            container.run();
        }
    }

    public void stop() {
        for (Container container : compose) {
            container.stop();
        }
    }
    
}