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

import java.awt.Color;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import kilim.Pausable;
import erjang.EBinary;
import erjang.EHandle;
import erjang.EObject;
import erjang.EPID;
import erjang.EString;
import erjang.driver.EDriverInstance;
import erjang.driver.IO;

public class TTYTextAreaDriverControl extends EDriverInstance implements
		KeyListener {

	/* The various opcodes. */
	static final int OP_PUTC = 0;
	static final int OP_MOVE = 1;
	static final int OP_INSC = 2;
	static final int OP_DELC = 3;
	static final int OP_BEEP = 4;
	/* Control op */
	static final int CTRL_OP_GET_WINSIZE = 100;
	static final int CTRL_OP_GET_UNICODE_STATE = 101;
	static final int CTRL_OP_SET_UNICODE_STATE = 102;

	static final int CONTROL_TAG = 0x10000000; /*
												 * Control character, value in
												 * first position
												 */
	static final int ESCAPED_TAG = 0x01000000; /*
												 * Escaped character, value in
												 * first position
												 */
	static final int TAG_MASK = 0xFF000000;

	static final int MAXSIZE = 1 << 16;

	private JTextPane area;
	private SimpleAttributeSet promptStyle;
	private SimpleAttributeSet inputStyle;
	private SimpleAttributeSet outputStyle;
	private SimpleAttributeSet resultStyle;
	private int startPos;
	private boolean utf8_mode = true;
	private Clipboard clipboard;
	private SimpleAttributeSet errorStyle;

	private static final int MAX_DOC_SIZE = 100000;

	public TTYTextAreaDriverControl(TTYTextAreaDriver driver, JTextPane text,
			String message) {
		super(driver);

		this.clipboard = text.getToolkit().getSystemClipboard();

		this.area = text;

		// inputJoin.send(Channel.EMPTY, null);

		text.addKeyListener(this);

		text.setAutoscrolls(true);
		
		// No editing before startPos
		if (text.getDocument() instanceof AbstractDocument)
			((AbstractDocument) text.getDocument())
					.setDocumentFilter(new DocumentFilter() {
						public void insertString(
								DocumentFilter.FilterBypass fb, int offset,
								String string, AttributeSet attr)
								throws BadLocationException {
							if (offset >= startPos)
								super.insertString(fb, offset, string, attr);
						}

						public void remove(DocumentFilter.FilterBypass fb,
								int offset, int length)
								throws BadLocationException {
							if (offset >= startPos || offset == 0)
								super.remove(fb, offset, length);
						}

						public void replace(DocumentFilter.FilterBypass fb,
								int offset, int length, String text,
								AttributeSet attrs) throws BadLocationException {
							if (offset >= startPos)
								super.replace(fb, offset, length, text, attrs);
						}
					});

		promptStyle = new SimpleAttributeSet();
		StyleConstants.setForeground(promptStyle, new Color(0xa4, 0x00, 0x00));

		inputStyle = new SimpleAttributeSet();
		StyleConstants.setForeground(inputStyle, new Color(0x20, 0x4a, 0x87));

		outputStyle = new SimpleAttributeSet();
		StyleConstants.setForeground(outputStyle, Color.darkGray);

		errorStyle = new SimpleAttributeSet();
		StyleConstants.setForeground(errorStyle, Color.red);

		resultStyle = new SimpleAttributeSet();
		StyleConstants.setItalic(resultStyle, true);
		StyleConstants.setForeground(resultStyle, new Color(0x20, 0x4a, 0x87));

		if (message != null) {
			final MutableAttributeSet messageStyle = new SimpleAttributeSet();
			StyleConstants.setBackground(messageStyle, text.getForeground());
			StyleConstants.setForeground(messageStyle, text.getBackground());
			append(message, messageStyle);
		}

		startPos = text.getDocument().getLength();
	}

	public InputStream getInputStream() {
		return System.in;
	}

	public OutputStream getOutputStream() {
		return new Output(outputStyle);
	}

	public OutputStream getErrorStream() {
		return new Output(errorStyle);
	}

	class Output extends OutputStream {
		
		private AttributeSet style;

		public Output(AttributeSet style) {
			this.style = style;
		}
		
		@Override
		public void write(int b) throws IOException {
			String data = new String(new byte[] { (byte) b }, IO.UTF8);
			append(data, style);
		}
		
		@Override
		public void write(byte[] b, int off, int len) throws IOException {
			String data = new String(b,off,len,IO.UTF8);
			append(data, style);
		}
	}
	

	
	protected void append(final String toAppend, final AttributeSet style) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				try {
					Document doc = area.getDocument();
					doc.insertString(doc.getLength(), toAppend, style);
					area.setCaretPosition(doc.getLength());
//					area.select(area.getCaretPosition(), area.getCaretPosition());

					// Cut the document to fit into the MAX_DOC_SIZE.
					// See JRUBY-4237.
					int extra = doc.getLength() - MAX_DOC_SIZE;
					if (extra > 0) {
						int removeBytes = extra + MAX_DOC_SIZE / 10;
						doc.remove(0, removeBytes);
						startPos -= removeBytes;
					}
				} catch (BadLocationException e) {
				}
			}
		});
	}

	@Override
	protected void output(EHandle caller, ByteBuffer buf) throws IOException,
			Pausable {

		// if (lpos > MAXSIZE)
		// put_chars("\n");

		switch (buf.get()) {
		case OP_PUTC:
			put_chars(buf);
			break;
		case OP_MOVE:
			move_rel(buf.getShort());
			break;
		case OP_INSC:
			ins_chars(buf);
			break;
		case OP_DELC:
			del_chars(buf.getShort());
			break;
		case OP_BEEP:
			visible_beep();
			break;
		default:
			/* Unknown op, just ignore. */
			break;
		}
		return; /* TRUE; */
	}

	private void visible_beep() {
		{
			Color fg = area.getForeground();
			Color bg = area.getBackground();

			area.setForeground(bg);
			area.setBackground(fg);

			area.repaint();
			Toolkit.getDefaultToolkit().beep();

			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
			}

			area.setForeground(fg);
			area.setBackground(bg);
			area.repaint();
		}
	}

	private void del_chars(int i) {

		try {
			Document doc = area.getDocument();
			int offs = area.getCaretPosition() + i;
			doc.remove(offs, -i);
		} catch (BadLocationException e) {
		}

	}

	private void ins_chars(ByteBuffer buf) {
		final String string = new String(buf.array(), buf.arrayOffset()
				+ buf.position(), buf.remaining(), IO.UTF8);

		SwingUtilities.invokeLater(new Runnable() {
			public void run() {

				try {
					Document doc = area.getDocument();
					int pos = area.getCaretPosition();
					doc.insertString(pos, string, inputStyle);
					area.setCaretPosition(pos + string.length());
					area.select(area.getCaretPosition(), area.getCaretPosition());

					// Cut the document to fit into the MAX_DOC_SIZE.
					// See JRUBY-4237.
					int extra = doc.getLength() - MAX_DOC_SIZE;
					if (extra > 0) {
						int removeBytes = extra + MAX_DOC_SIZE / 10;
						doc.remove(0, removeBytes);
						startPos -= removeBytes;
					}
				} catch (BadLocationException e) {
				}

			}
		});

	}

	private void move_rel(final int delta_pos) {
		if (delta_pos != 0) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					int curr = area.getCaretPosition();
					area.setCaretPosition(curr + delta_pos);					
					area.select(area.getCaretPosition(), area.getCaretPosition());
				}
			});
		}
	}

	private void put_chars(ByteBuffer buf) {
		String string = new String(buf.array(), buf.arrayOffset()
				+ buf.position(), buf.remaining(), IO.UTF8);
		append(string, outputStyle);
	}

	private void put_chars(String string) {
		append(string, outputStyle);
	}

	@Override
	public void keyTyped(KeyEvent e) {
		e.consume();
	}

	@Override
	public void keyReleased(KeyEvent e) {
	}

	@Override
	public void keyPressed(KeyEvent event) {
		int code = event.getKeyCode();
		if (code == KeyEvent.VK_COPY
				|| (code == KeyEvent.VK_C && event.isMetaDown())) {
			return;
		}

		if (code == KeyEvent.VK_PASTE
				|| (code == KeyEvent.VK_V && event.isMetaDown())) {

			Transferable clipData = clipboard.getContents(clipboard);
			try {
				if (clipData.isDataFlavorSupported(DataFlavor.stringFlavor)) {
					String s = (String) (clipData
							.getTransferData(DataFlavor.stringFlavor));
					out(s);
				}
			} catch (Exception ufe) {
			}

			event.consume();
			return;
		}

		if (code == KeyEvent.VK_CUT
				|| (code == KeyEvent.VK_X && event.isMetaDown())) {
			event.consume();
			visible_beep();
			return;
		}

		
		
		event.consume();
		switch (code) {
		case KeyEvent.VK_TAB:
			out('\t');
			return;
		case KeyEvent.VK_ESCAPE:
			out_byte((byte) 27);
			return;

		case KeyEvent.VK_LEFT:
			out_ctrl('b');
			return;
		case KeyEvent.VK_DELETE:
			out_ctrl('d');
			return;
		case KeyEvent.VK_RIGHT:
			out_ctrl('f');
			return;
		case KeyEvent.VK_BACK_SPACE:
			out_ctrl('h');
			return;
		case KeyEvent.VK_ENTER:
			out_ctrl('j');
			return;
		case KeyEvent.VK_DOWN:
			out_ctrl('n');
			return;
		case KeyEvent.VK_UP:
			out_ctrl('p');
			return;
		}

		if (event.getKeyChar() == KeyEvent.CHAR_UNDEFINED) {
		//	System.err.println("ignored key event: " + event);
			return;
		}

		if ((event.getModifiersEx() & KeyEvent.CTRL_DOWN_MASK) != 0) {
		//	System.err.println("ctrl event: " + event);
			if (code >= KeyEvent.VK_A && code <= KeyEvent.VK_Z) {
				out_ctrl('a' + code - KeyEvent.VK_A);
			}

		} else {
			out(event.getKeyChar());
		}
	}

	private void out_ctrl(int i) {
		if (i >= 'a' && i <= 'z') {
			out_byte((byte) ((i - 'a') + 1));
		}
	}

	private void out_byte(int b) {
		ByteBuffer buf = ByteBuffer.wrap(new byte[] { (byte) b });
		EObject out;

		if (task.send_binary_data()) {
			out = EBinary.make(buf);
		} else {
			out = EString.make(buf);
		}

		task.output_from_driver_b(out);
	}

	private void out(char ch) {
		String string = new String(new char[] { ch });
		out(string);
	}

	private void out(String string) {
		byte[] bytes = string.getBytes(IO.UTF8);
		ByteBuffer buf = ByteBuffer.wrap(bytes);

		// driver_output(bb);

		EObject out;

		if (task.send_binary_data()) {
			out = EBinary.make(buf);
		} else {
			out = EString.make(buf);
		}

		task.output_from_driver_b(out);
	}

	@Override
	protected ByteBuffer control(EPID pid, int command, ByteBuffer cmd)
			throws Pausable {

		if (command == CTRL_OP_GET_WINSIZE) {
			ByteBuffer rep = ByteBuffer.allocate(8);
			rep.order(ByteOrder.nativeOrder());

			rep.putInt(80);
			rep.putInt(25);
			return rep;

		} else if (command == CTRL_OP_GET_UNICODE_STATE) {
			ByteBuffer rep = ByteBuffer.allocate(1);
			rep.put((byte) (utf8_mode ? 1 : 0));
			return rep;

		} else if (command == CTRL_OP_SET_UNICODE_STATE && cmd.remaining() == 1) {
			ByteBuffer rep = ByteBuffer.allocate(1);
			rep.put((byte) (utf8_mode ? 1 : 0));
			utf8_mode = cmd.get() == 0 ? false : true;
			return rep;
		} else {
			return null;
		}

	}
}
