// 
// Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
// Copyright 2011 by José Albert Cruz Almaguer.
// 
// This program is licensed to you under the terms of version 3 of the
// GNU Affero General Public License. This program is distributed WITHOUT
// ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
// MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
// AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
// 
package manager;

import com.ericsson.otp.erlang.*;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ErlComInterface extends Thread {

    OtpNode node;
    OtpMbox mb;
    MainFrame frame;

    /**
     *
     * @param nodeName The name for the node, by convinience "pas" will be the
     * cookie.
     * @throws OtpErlangException
     * @throws IOException
     */
    public ErlComInterface(String nodeName, MainFrame frame) throws OtpErlangException, IOException {
        node = new OtpNode(nodeName, "pas");
        mb = node.createMbox("monitor");
        this.frame = frame;
    }

    @Override
    public void run() {
        try {

            OtpErlangTuple tup = (OtpErlangTuple) mb.receive();

            while (!tup.elementAt(0).toString().equals("fin")) {

                switch (tup.elementAt(0).toString()) {
                    case "list":
                        
                        String a = convertToRList((OtpErlangList) tup.elementAt(1));
                        String b = convertToRList((OtpErlangList) tup.elementAt(2));
//                        System.out.println(a);
                        frame.plot(a, b);
                        
                        break;
                }
                tup = (OtpErlangTuple) mb.receive();
            }

        } catch (OtpErlangExit | OtpErlangDecodeException ex) {
            Logger.getLogger(ErlComInterface.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private String convertToRList(OtpErlangList list) {
//        System.out.println(list);
        String res = "c(";
        for (int i = 1; i < list.arity() - 1; i++) {
            res += list.elementAt(i).toString() + ", ";
        }
        res += list.elementAt(list.arity() - 1).toString() + ")";
        return res;
    }
}
