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

/*************************************************
* DoseMonInfo.java - implements the 'Info' Dialog
*
* Implements the 'Info' Dialog
*
* Design:
* -------
* class DoseMonInfo
* {
*   private class DoseDialog extends JPanel
*   {
*     private void Show_Node_Info()
*     private class ButtonListener implements ActionListener
*     {
*        public void actionPerformed(ActionEvent event)
*     }
*               private DoseDialog(char cmd)
*   }
*   DoseMonInfo(char cmd)
* }
*
* Uses: DoseJni
*************************************************/
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JOptionPane;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
 
/*******************************************
* 
********************************************/

public class DoseMonInfo 
{
        // A class variable's value is shared by all instances of that class.
        static int m_Counter = 0;
        
        private class DoseDialog extends JPanel
        {
                JButton m_buttHelp;
                JButton m_buttInfo;
                JButton m_buttDbgPlus;
                JButton m_buttDbgMinus;
                JPanel  m_txtPan;
                JTextArea  m_txtArea;
                                
                /******************************************************
                *  Listener for all buttons in the Info-Dialog
                *******************************************************/
                
                private class ButtonListener implements ActionListener
                {
                        //================================
                        // not used
                        //================================
                        private void Show_Node_Info()
                        {
                                DoseJni doseJni = new DoseJni();
                                int rc;
                                String str = " ";
                                
                                byte[] buff = new byte[1024];
                
                                buff[5] = 17;
                                //System.out.println("*** call dose +++++++" );
                                // Assuming th lib is already loaded
                                rc = doseJni.GetInfo('D',4, buff);
                                //System.out.println("*** ret Dose ++++++++" + rc );
                                //for(byte ii = 0 ; ii< 8 ; ii++) System.out.print(" " + buff[ii]);
                        
                                for(byte ii = 0 ; ii< 8 ; ii++) 
                                        str += " " + buff[ii];
                        
                                System.out.println();

                                m_txtArea.append(str + " appended");                            
                        }
                        //==================================
                        // Called when butt Info is clicked
                        //==================================
                        private void Show_Node_Status()
                        {
                                DoseJni doseJni = new DoseJni();
                                int rc;
                                String str = " ";
                                byte[] buff = new byte[1024];

                                // Assuming th lib is already loaded
                                rc = doseJni.GetInfo('S',4, buff);

                                for(int ii = 0 ; ii<rc ; ii++) 
                                {
                                        str += (char)buff[ii];
                                }
                                m_txtArea.setText(str);
                        }

                        //====================================
                        // Increase/Decrease Debug in DoseCom
                        // Tested OK in Win32
                        //====================================
                        private void Set_Debug(int param)
                        {
                                DoseJni doseJni = new DoseJni();
                                int retValue;
                                
                                // Assuming th lib is already loaded
                                retValue = doseJni.GetInfo('D',param, null);

                                m_txtArea.setText("Debug = " + retValue);
                        }

                        //===================================
                        //
                        //==================================
                        public void actionPerformed(ActionEvent event)
                        {       
                                int retValue = 0;
                                
                                if(event.getSource() == m_buttHelp)
                                {
                                        m_txtArea.setText(" Help\n To be implemented ....");
                                        m_txtArea.setBackground(Color.yellow);
                                }
                                else //--------------------
                                if(event.getSource() == m_buttInfo)
                                {
                                        m_txtArea.setBackground(Color.yellow);
                                        Show_Node_Status();
                                }
                                else  //--------------------
                                if(event.getSource() == m_buttDbgPlus)
                                {
                                        Set_Debug(1);
                                }
                                else  //--------------------
                                if(event.getSource() == m_buttDbgMinus)
                                {
                                        Set_Debug(-1);
                                }
                        }
                }

                //====================================================
                // main constructor
                //====================================================
                private DoseDialog(char cmd)
                {
                        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));       
                
                        //---------- Buttons -------

                        JPanel pan = new JPanel(); add(pan);
                        pan.setLayout(new BoxLayout(pan, BoxLayout.X_AXIS));    // 2 i mitten

                        m_buttHelp     = new JButton("Help"); pan.add(m_buttHelp);
                        m_buttInfo     = new JButton("GetInfo"); pan.add(m_buttInfo);
                        m_buttDbgPlus  = new JButton("Dbg++"); pan.add(m_buttDbgPlus);
                        m_buttDbgMinus = new JButton("Dbg--"); pan.add(m_buttDbgMinus);

                        ButtonListener buttonListener = new ButtonListener();
                        m_buttHelp.addActionListener(buttonListener);
                        m_buttInfo.addActionListener(buttonListener);
                        m_buttDbgPlus.addActionListener(buttonListener);
                        m_buttDbgMinus.addActionListener(buttonListener);

                        //---------- Text Area -------

                        m_txtArea = new JTextArea("Text",30,30); // rows,cols           
                        add(m_txtArea);         
                        m_txtArea.setBackground(Color.green);
                        
                    Font font = new Font("Courier new", Font.ROMAN_BASELINE, 12);
            m_txtArea.setFont(font);                    
                }
        }

        //==============================
        // constructor
        //==============================
        DoseMonInfo(char cmd)
        {
                JFrame frame = new JFrame("Info");
                //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);         
                DoseDialog info = new DoseDialog(cmd);
                frame.getContentPane().add(info);
                frame.setVisible(true);
                frame.pack();
        }
}
/*------------------------ end DoseMonInfo.cpp ----------------------*/
