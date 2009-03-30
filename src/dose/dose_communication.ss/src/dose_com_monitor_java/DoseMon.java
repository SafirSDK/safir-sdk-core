/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
* 
* Created by: Lars Engdahl / stlsen
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.*;
import javax.swing.JOptionPane;

/****************************************
* Main Class
*****************************************/
class DoseMon
{
   public static void main(String[] args)
    {
        byte[] buff = new byte[2048];
        byte[] ip = new byte[4];

        System.out.println("-------------------- DoseMon Start" );

        JFrame frame = new JFrame("DoseMon");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        DoseMonPanel panel = new DoseMonPanel();

        frame.getContentPane().add(panel);

        // --- Testing ----
/*-----
        ip[0] = 3;  ip[1] = 4; ip[2] = 5; ip[3] = 7;
        panel.updateNode('U', (byte)24, ip);

        ip[0] = 30; ip[1] = 40; ip[2] = 50; ip[3] = 70;

        panel.updateNode('U', (byte)34, ip);

        panel.updateNode('D', (byte) 34, ip);

----*/
        frame.pack(); // resize window to exact fit

        frame.setVisible(true);

        // --- Test JNI to DoseComDll ---

        DoseJni doseJni = new DoseJni();
        int rc = doseJni.loadDoseLib();

                if(rc == -1) // failed
                        JOptionPane.showMessageDialog(null,
                                "ERROR: DoseMon can not load file.\n"
                                + "Filename LINUX:  libdose_com_jni.so\n"
                                + "Filename WIN32:  dose_com_jni.dll\n");
        
                rc = doseJni.GetInfo('N',4, buff);
        //System.out.println("*** ret Dose " + rc );

        //for(byte ii = 0 ; ii< 8 ; ii++) System.out.print(" " + buff[ii]);
        //System.out.println();

        // --- Test Sleep and add nodes ---

        //---------------------------------------------------------
        // Loop Sleep 1 sec, get info about nodes and update graphs
        //---------------------------------------------------------

        for(;;)
        {
            int numNodes;

            try
            {
                Thread.sleep(1000);
            } catch (InterruptedException e)
            {
                e.printStackTrace();
            }

            //System.out.println("*** Main call dose" );
            numNodes = doseJni.GetInfo('N',4, buff);
            //System.out.println("*** Main ret Dose. numNodes = " + numNodes );

            //for(byte ii = 0 ; ii< 8 ; ii++) System.out.print(" " + buff[ii]);

            // Must agree with C-code in DoseComJni.cpp - JavaDoseJni_GetInfo()
            for( int num=0 ; num <numNodes ; num++)
            {
                ip[0] = buff[8*num + 4];
                ip[1] = buff[8*num + 5];
                ip[2] = buff[8*num + 6];
                ip[3] = buff[8*num + 7];

                // System.out.print(" " + buff[ii]);
                //System.out.println("Got "
                //      + buff[8*num]   + " "   + buff[8*num+1] + " "
                //      + buff[8*num+4] + " " + buff[8*num+5] + " "
                //      + buff[8*num+6] + " " + buff[8*num+7] + " ");

                panel.updateNode((char) buff[8*num + 1], // status
                                  (char) buff[8*num + 3], //xtrastatus
                                  buff[8*num],  // DoseId
                                  ip);          // ipAddress
            }
            frame.pack(); // resize window to exact fit
        } // end for(;;)
    }
}
