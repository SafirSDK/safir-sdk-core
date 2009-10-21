// -*- coding: utf-8 -*-
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
import javax.swing.JTextField;
import javax.swing.JOptionPane;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.BorderFactory;
import javax.swing.border.Border;

/*******************************************
*
********************************************/

public class DoseMonPanel  extends JPanel
{
        OneDoseNode[] m_allNodes;
        JButton m_buttHelp;
        JButton m_buttInfo;

        /******************************************************
        *
        *******************************************************/
        private class ButtonListener implements ActionListener
        {
                public void actionPerformed(ActionEvent event)
                {
                        if(event.getSource() == m_buttHelp)
                        {
                                JOptionPane.showMessageDialog(null,
                                "Help\n"
                                + "Columns:\n"
                                + "ID = DoseId\n"
                                + "     DoseMon do not know which DoseId are expected to be present.\n"
                                + "     Only Dose nodes that announce themselfs on the network are shown.\n"
                                + "\n"
                                + "State:\n"
                                + "   Me   - (YELLOW) This node.\n"
                                + "   New  - (BLUE)  The node is present but has not sent (or is sending) PD.\n"
                                + "   Up   - (GREEN) The node is present and has completed PD.\n"
                                + "   Down - (RED)   The node has been Up or New, but has disappeared.\n"
                                + "\n"
                                + "x:\n"
                                + "   The color indicates what the node thinks of this node (Me).\n"
                                + "   BLUE  - This node (Me) has NOT completed PD to the actual node.\n"
                                + "   GREEN - This node (Me) has completed PD to the actual node.\n"
                                + "   Normally, blue is a temporary state at startup. Green is OK.\n");
                        }
                        else
                        if(event.getSource() == m_buttInfo)
                        {
                                DoseMonInfo info = new DoseMonInfo('I');
                        }
                }
        }
        /******************************************************
        *  Holds info of one node - one instance for each node
        *******************************************************/
        private class OneDoseNode
        {
                JTextField m_txt_nid;
                JTextField m_txt_ip;
                JTextField m_txt_status;
                JTextField m_txt_xtra;

                byte    m_ipAddr[] = new byte[4];
                char    m_nodeStatus;
                byte    m_doseId;

                //=======================
                // constructor
                //=======================
                OneDoseNode(byte doseId, char nodeStatus, byte[] ipAddr)
                {
                        m_doseId = doseId;
                        m_ipAddr = ipAddr;
                        m_nodeStatus = nodeStatus;
                }
        }

        //=======================
        // main constructor
        //=======================
        public DoseMonPanel()
        {
                m_allNodes = new OneDoseNode[64];

        //setLayout(new GridLayout(11,1));      - blir ej bra
                setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

                //---------- Buttons -------

                JPanel p1 = new JPanel(); add(p1);
                p1.setLayout(new BoxLayout(p1, BoxLayout.X_AXIS)); // 2 i mitten

                m_buttHelp = new JButton("Help"); p1.add(m_buttHelp);
                m_buttInfo = new JButton("Info"); p1.add(m_buttInfo);

                ButtonListener buttonListener = new ButtonListener();
                m_buttHelp.addActionListener(buttonListener);
                m_buttInfo.addActionListener(buttonListener);

                //---------- header -------

                addRow((byte) 99, 'H', "ID");
        }
        //=======================
        //
        //=======================
        private void addRow(byte doseId, char mode,     String strDoseId)
                                //String strDoseId, String ipAddr, String nodeStatus)
        {
                JPanel pan = new JPanel(); add(pan);
                pan.setLayout(new GridBagLayout());

                JTextField  t11 = new JTextField(strDoseId,2);  pan.add(t11);
                JTextField  t22 = new JTextField(" ",1);                pan.add(t22);
                JTextField  t33 = new JTextField("xx",10);      pan.add(t33);
                JTextField  t44 = new JTextField("yy",4);           pan.add(t44);

            t11.setForeground(Color.black);
            t22.setForeground(Color.black);
            t33.setForeground(Color.black);
            t44.setForeground(Color.black);

                if(mode == 'H') // Header
                {
                        t11.setBackground(null);
                        t22.setBackground(null);
                        t33.setBackground(null);
                        t44.setBackground(null);
                        t22.setText("x");

                    t33.setText("IpAddress");
                    t44.setText("State");
                }

                if(doseId >= 64) return;

                // Save so they can be updated

                m_allNodes[doseId].m_txt_nid            = t11;
                m_allNodes[doseId].m_txt_xtra           = t22;
                m_allNodes[doseId].m_txt_ip             = t33;
                m_allNodes[doseId].m_txt_status         = t44;
        }
        //=======================
        //
        //=======================

        public void updateNode(char nodeStatus, char xtraStatus,
                            byte doseId, byte[] ipAddr)
        {
                String ipStr = "";
                int ip_Addr;

                // must do this since 'byte' is unsigned
                for(int jj=0 ; jj<4 ; jj++)
                {
                        ip_Addr = ipAddr[jj];
                        if(ip_Addr<0) ip_Addr += 256;
                        ipStr += ip_Addr;
                        if(jj<3) ipStr += ".";
                }

                if(m_allNodes[doseId] == null)
                {
                        m_allNodes[doseId]
                                = new OneDoseNode(doseId, nodeStatus, ipAddr);

                        addRow(doseId, nodeStatus, " " + doseId);
                }

                // What to do here is to set/change text in a TextField

                String nsStr = " ";

        // Color in ID field depends on NodeStatus
                switch(nodeStatus)
                {
                        case 'U':
                m_allNodes[doseId].m_txt_nid.setBackground(Color.green);
                            nsStr = " Up";
                break;
                        case 'D':
                m_allNodes[doseId].m_txt_nid.setBackground(Color.red);
                            nsStr = " Down";
                break;
                        case 'N':
                m_allNodes[doseId].m_txt_nid.setBackground(Color.blue);
                nsStr = " New";
                break;
                        case 'M':
                                m_allNodes[doseId].m_txt_nid.setBackground(Color.yellow);
                            nsStr = " Me";
                                break;
                }

                m_allNodes[doseId].m_txt_ip.setText(ipStr);
                m_allNodes[doseId].m_txt_status.setText(nsStr);

                // The xtra field has a color that this node thinks the remote
        // thinks of this node.
                switch(xtraStatus)
                {
                        case 'U':
                m_allNodes[doseId].m_txt_xtra.setBackground(Color.green); break;
                        case 'N':
                m_allNodes[doseId].m_txt_xtra.setBackground(Color.blue);  break;
                        case 'M':
                                m_allNodes[doseId].m_txt_xtra.setBackground(Color.yellow); break;
                }
        }
}
