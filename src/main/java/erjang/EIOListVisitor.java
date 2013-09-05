package erjang;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.Checksum;

import erjang.driver.IO;
import erjang.driver.IO.BARR;

public abstract class EIOListVisitor {
	
	public void visit(ByteBuffer buf) {
		visit(buf.array(), buf.arrayOffset() + buf.position(), buf.remaining());
	}

	public void add(byte[] array) {
		visit(array, 0, array.length);
	}
	
	public void visit(byte[] array, int off, int len) {
		for (int i = 0; i < len; i++) {
			visit(array[off + i]);
		}
	}

	abstract void visit(int aByte);
	
	
	public static long size( EObject io_list ) {
		
		final long[] size = new long[1];
		
		io_list.visitIOList( new EIOListVisitor() {
			
			@Override
			public void visit(ByteBuffer buf) {
				size[0] += buf.remaining();
			}
			
			@Override
			public void add(byte[] array) {
				size[0] += array.length;
			}
			
			@Override
			public void visit(byte[] array, int off, int len) {
				size[0] += len;
			}
			
			@Override
			void visit(int aByte) {
				size[0] += 1;
			}
			
		});
		
		return size[0];
	}

	public static ByteBuffer[] collectv( EObject io_list ) {
		
		final List<ByteBuffer> list = new ArrayList<ByteBuffer>();
		
		final IO.BARR[] barr = new IO.BARR[1];
		
		io_list.visitIOList( new EIOListVisitor() {

			private void flush() {
				if (barr[0] != null) {
					list.add(barr[0].wrap());
					barr[0] = null;
				}
			}
			
			@Override
			public void visit(ByteBuffer buf) {
				flush();
				list.add(buf);
			}
			
			@Override
			public void visit(byte[] array, int off, int len) {
				if (barr[0] != null && len < 32) {
					barr[0].write(array, off, len);
				} else {
					flush();
					list.add(ByteBuffer.wrap(array, off, len));
				}
			}
			
			@Override
			void visit(int aByte) {
				if (barr[0] == null) { barr[0] = new BARR(); }
				barr[0].write(aByte);
			}
			
		});
		
		if (barr[0] != null) {
			list.add(barr[0].wrap());
			barr[0] = null;
		}
		
		return list.toArray(new ByteBuffer[ list.size() ]);
	}

	public static ByteBuffer collect( EObject io_list ) {
		
		EBinary bin;
		if ((bin = io_list.testBinary()) != null) {
			return bin.toByteBuffer();
		}
		
		@SuppressWarnings("resource")
		final BARR out = new BARR();
		io_list.visitIOList( new EIOListVisitor() {
			
			@Override
			public void visit(byte[] array, int off, int len) {
				out.write(array, off, len);
			}
			
			@Override
			void visit(int aByte) {
				out.write(aByte);
			}
			
		});
		return out.wrap();
	}
	
	public static void update( EObject io_list, final Checksum checksum ) {
		io_list.visitIOList( new EIOListVisitor() {
			
			@Override
			public void visit(byte[] array, int off, int len) {
				checksum.update(array, off, len);
			}
			
			@Override
			void visit(int aByte) {
				checksum.update(aByte);
			}
			
		});
	}
}
