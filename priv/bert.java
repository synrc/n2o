
/* run tests: javac bert.java ; java -classpath . bert */

import java.nio.*;
import java.util.ArrayList;
public class bert {
    public int t;
    public Object v;
    public bert(int t, Object v) { this.t = t; this.v = v; }
    public static bert float_ (float o)      { return new bert(70, o); }
    public static bert tuple  (ArrayList o)  { return new bert(104,o); }
    public static bert string (String o)     { return new bert(107,o); }
    public static bert list   (ArrayList o)  { return new bert(108,o); }
    public static bert bin    (ByteBuffer o) { return new bert(109,o); }
    public static bert atom   (String o)     { return new bert(100,o); }
    public static bert number (ArrayList o)  { return new bert(110,o); }
    public static bert map    (ArrayList o)  { return new bert(116,o); }
    public static void main(String[] args) { System.out.println("BERT for Java (c) SYNRC 2019"); }
}
