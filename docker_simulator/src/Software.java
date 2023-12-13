public class Software {
    String name;
    int size;
    public Software(String name, int size){
        this.name = name;
        this.size = size;
        System.out.println("Software added: '" + this.name + "' with size: " + this.size);
    }

    public String getName() {
        return this.name;
    }

    public String toString() {
        return "Name: '" + this.name + "' Size : " + this.size;
    }

}