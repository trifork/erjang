package erjang;

import java.util.Iterator;
import java.util.NoSuchElementException;


public class EObjectIterator implements Iterator<EObject> {
	private EObject object = null;

	public EObjectIterator(EObject object) {
		this.object = object;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		if (object == null) {
			return false;
		}
		return !object.isNil();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public EObject next() {
		if ((object == null)
			|| object.isNil()) {
			throw new NoSuchElementException();
		}
		
		EObject next;
		
		ECons cons = object.testCons();
		if ((cons != null)
			// TODO how to handle EString? return each character? Or return it as single element only?
			// for now we don't treat EString as collection, but as single element
			&& !(cons instanceof EString)) {
			next = cons.head();
			object = cons.tail();
		}
		else {
			// simple object, return it and set next object to null
			next = object;
			object = null;
		}
		return next;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		// not supported
		throw new UnsupportedOperationException();
	}
}
