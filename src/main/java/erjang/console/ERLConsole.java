/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package erjang.console;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.SystemColor;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Arrays;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import erjang.ERT;
import erjang.util.Progress;
import erjang.util.ProgressListener;


public class ERLConsole extends JFrame {
    public ERLConsole(String title) {
        super(title);
    }
    
    static final int HINSET = 8;

    public static void main(final String[] args) {
        final ERLConsole console = new ERLConsole("Erjang Console");

        console.getContentPane().setLayout(new BorderLayout());
        console.setSize(700, 600);

        final JTextPane text = new JTextPane();

        text.setMargin(new Insets(8,HINSET,8,HINSET));
        text.setCaretColor(new Color(0xa4, 0x00, 0x00));
        text.setBackground(new Color(0xf2, 0xf2, 0xf2));
        text.setForeground(new Color(0xa4, 0x00, 0x00));
        Font font = console.findFont("Monospaced", Font.PLAIN, 14,
                new String[] {"Monaco", "Andale Mono"});
        
        text.setFont(font);
        JScrollPane pane = new JScrollPane();
        pane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        pane.setViewportView(text);
        pane.setBorder(BorderFactory.createLineBorder(Color.darkGray));
        console.getContentPane().add(pane, BorderLayout.CENTER);
        
        StatusBar status = new StatusBar();
        console.getContentPane().add(status, BorderLayout.SOUTH);
                
        console.validate();

        final TTYTextAreaDriver tty = new TTYTextAreaDriver(text, " Welcome to the Erjang Console \n\n");
        console.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                ERT.shutdown();
            }
        });

        erjang.OTPMain.add_driver(tty);

        Thread t2 = new Thread() {
            public void run() {
                console.setVisible(true);
                try {
					erjang.Main.main(args);
				} catch (Exception e) {
					e.printStackTrace();
				}
            }
        };
        t2.start();

        try {
            t2.join();
        } catch (InterruptedException ie) {
            // ignore
        }

        System.exit(0);
    }

    private Font findFont(String otherwise, int style, int size, String[] families) {
        String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
        Arrays.sort(fonts);
        Font font = null;
        for (int i = 0; i < families.length; i++) {
            if (Arrays.binarySearch(fonts, families[i]) >= 0) {
                font = new Font(families[i], style, size);
                break;
            }
        }
        if (font == null)
            font = new Font(otherwise, style, size);
        return font;
    }

    /**
     *
     */
    private static final long serialVersionUID = 3746242973444417387L;

}


@SuppressWarnings("serial")
class StatusBar extends JPanel {

	  private JLabel mem_label;
	private JLabel progress;

	public StatusBar() {
	    setLayout(new BorderLayout());
	    setPreferredSize(new Dimension(10, 23));

	    JPanel rightPanel = new JPanel(new BorderLayout());
	    rightPanel.add(new JLabel(new AngledLinesWindowsCornerIcon()), BorderLayout.SOUTH);
	    rightPanel.setOpaque(false);
	    add(rightPanel, BorderLayout.EAST);
	    setBackground(SystemColor.control);
	    
	    progress = new JLabel("booting...");
	    progress.setPreferredSize(new Dimension(300,23));
	    Progress.setListener(new ProgressListener() {
			
			@Override
			public void progress(final String msg) {
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						progress.setText(msg);
						progress.repaint();
					}});
			}
		});
	    add(progress, BorderLayout.WEST);
	    
	    mem_label = new JLabel();
	    add(mem_label, BorderLayout.CENTER);
	    
	    new Timer(true).schedule(new TimerTask() {

			@Override
			public void run() {
				StatusBar.this.updateMemory();
			}}, 1000, 1000);
	  }

	  void updateMemory() {
		  double total = Runtime.getRuntime().totalMemory() / (1024.0 * 1024.0);
		  double free = Runtime.getRuntime().freeMemory() / (1024.0 * 1024.0);
		  String msg = "" + ((int)(total-free)) + "/" +((int)total)+ "MB";
		  mem_label.setText(msg);
	  }
	  
	  protected void paintComponent(Graphics g) {
	    super.paintComponent(g);

	    int y = 0;
	    g.setColor(new Color(156, 154, 140));
	    g.drawLine(0, y, getWidth(), y);
	    y++;
	    g.setColor(new Color(196, 194, 183));
	    g.drawLine(0, y, getWidth(), y);
	    y++;
	    g.setColor(new Color(218, 215, 201));
	    g.drawLine(0, y, getWidth(), y);
	    y++;
	    g.setColor(new Color(233, 231, 217));
	    g.drawLine(0, y, getWidth(), y);

	    y = getHeight() - 3;
	    g.setColor(new Color(233, 232, 218));
	    g.drawLine(0, y, getWidth(), y);
	    y++;
	    g.setColor(new Color(233, 231, 216));
	    g.drawLine(0, y, getWidth(), y);
	    y = getHeight() - 1;
	    g.setColor(new Color(221, 221, 220));
	    g.drawLine(0, y, getWidth(), y);

	  }

	}

	class AngledLinesWindowsCornerIcon implements Icon {
	  private static final Color WHITE_LINE_COLOR = new Color(255, 255, 255);

	  private static final Color GRAY_LINE_COLOR = new Color(172, 168, 153);
	  private static final int WIDTH = 13;

	  private static final int HEIGHT = 13;

	  public int getIconHeight() {
	    return WIDTH;
	  }

	  public int getIconWidth() {
	    return HEIGHT;
	  }

	  public void paintIcon(Component c, Graphics g, int x, int y) {

	    g.setColor(WHITE_LINE_COLOR);
	    g.drawLine(0, 12, 12, 0);
	    g.drawLine(5, 12, 12, 5);
	    g.drawLine(10, 12, 12, 10);

	    g.setColor(GRAY_LINE_COLOR);
	    g.drawLine(1, 12, 12, 1);
	    g.drawLine(2, 12, 12, 2);
	    g.drawLine(3, 12, 12, 3);

	    g.drawLine(6, 12, 12, 6);
	    g.drawLine(7, 12, 12, 7);
	    g.drawLine(8, 12, 12, 8);

	    g.drawLine(11, 12, 12, 11);
	    g.drawLine(12, 12, 12, 12);

	  }
	}

	           
