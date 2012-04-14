package erjang;

import java.util.Iterator;
import java.util.NoSuchElementException;

import junit.framework.TestCase;

public class EObjectIteratorTest extends TestCase {
	public void testNil() throws Exception {
		Iterator<EObject> it = new EObjectIterator(ERT.NIL);
		assertNotNull(it);
		assertFalse(it.hasNext());
	}
	
	public void testNull() throws Exception {
		Iterator<EObject> it = new EObjectIterator(null);
		assertNotNull(it);
		assertFalse(it.hasNext());
	}
	
	public void testEString() throws Exception {
		EString message = EString.fromString("Hello, World!");
		Iterator<EObject> it = new EObjectIterator(message);
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertEquals(message, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
	
	public void testEInt() throws Exception {
		EInteger num = ESmall.make(42);
		Iterator<EObject> it = new EObjectIterator(num);
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertEquals(num, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
	
	public void testSeqWithSingleNil() throws Exception {
		ESeq seq = ESeq.fromArray(new EObject[] {
				ERT.NIL
		});
		assertEquals(1, seq.length());
		Iterator<EObject> it = seq.iterator();
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertTrue(o.isNil());
		assertEquals(ERT.NIL, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
	
	public void testSeqWithTwoNils() throws Exception {
		ESeq seq = ESeq.fromArray(new EObject[] {
				ERT.NIL,
				ERT.NIL
		});
		assertEquals(2, seq.length());
		Iterator<EObject> it = seq.iterator();
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertTrue(o.isNil());
		assertEquals(ERT.NIL, o);
		assertTrue(it.hasNext());
		o = it.next();
		assertNotNull(o);
		assertTrue(o.isNil());
		assertEquals(ERT.NIL, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
	
	public void testSeqWithSingleObject() throws Exception {
		EObject first = EString.fromString("Hello");
		ESeq seq = ESeq.fromArray(new EObject[] {
				first
		});
		assertEquals(1, seq.length());
		Iterator<EObject> it = seq.iterator();
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertEquals(first, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
	
	public void testSeqWithTwoObjects() throws Exception {
		EObject first = EString.fromString("Hello");
		EObject second = EString.fromString("World");
		ESeq seq = ESeq.fromArray(new EObject[] {
				first,
				second
		});
		assertEquals(2, seq.length());
		Iterator<EObject> it = seq.iterator();
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertEquals(first, o);
		assertTrue(it.hasNext());
		o = it.next();
		assertNotNull(o);
		assertEquals(second, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
	
	public void testSeqWithThreeObjects() throws Exception {
		EObject first = EString.fromString("Hello");
		EObject second = EString.fromString("World");
		EObject third = ESmall.make(42);
		ESeq seq = ESeq.fromArray(new EObject[] {
				first,
				second,
				third
		});
		assertEquals(3, seq.length());
		Iterator<EObject> it = seq.iterator();
		assertNotNull(it);
		assertTrue(it.hasNext());
		EObject o = it.next();
		assertNotNull(o);
		assertEquals(first, o);
		assertTrue(it.hasNext());
		o = it.next();
		assertNotNull(o);
		assertEquals(second, o);
		assertTrue(it.hasNext());
		o = it.next();
		assertNotNull(o);
		assertEquals(third, o);
		assertFalse(it.hasNext());
		try {
			it.next();
			fail("no more elements, iterator should throw exception");
		}
		catch (NoSuchElementException t) {
			// expected
		}
	}
}
