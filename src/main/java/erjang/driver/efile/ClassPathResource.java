package erjang.driver.efile;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Calendar;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import kilim.Pausable;
import erjang.EBinary;
import erjang.driver.IO;

public class ClassPathResource {

	public static EBinary read_file(String name) {

		if (!name.startsWith(EFile.RESOURCE_PREFIX))
			return null;

		InputStream resource = ClassPathResource.class.getClassLoader()
				.getResourceAsStream(
						name.substring(EFile.RESOURCE_PREFIX.length()));

		if (resource == null) {
			return null;
		} else {
			EBinary bin = null;
			try {
				bin = IO.istream2binary(resource);
				return bin;
			} catch (IOException e) {
				return null;
			} finally {
				try {
					resource.close();
				} catch (IOException e) {
				}
			}
		}

	}

	public static void listdir(EFile eFile, String path) throws Pausable {

		String[] dir;

		try {
			dir = list(path);
		} catch (IOException e) {
			eFile.reply_posix_error(IO.exception_to_posix_code(e));
			return;
		}

		eFile.reply_list_directory(dir);

	}

	static String[] list(String path) throws IOException {
		Enumeration<URL> out = ClassPathResource.class.getClassLoader()
				.getResources(path);

		Set<String> res = new HashSet<String>();

		while (out.hasMoreElements()) {
			URL u = out.nextElement();
			list(res, u);
		}

		return res.toArray(new String[res.size()]);
	}

	static void list(Set<String> res, URL url) throws IOException {

		if (url.getProtocol().equals("jar")) {
			listJarURL(res, url);
		}
		if (url.getProtocol().equals("file")) {
			File file = new File(url.getFile());
			for (String elm : file.list()) {
				res.add(elm);
			}
		} else {
			return;
		}

	}

	private static void listJarURL(Set<String> res, URL url) throws IOException {
		String path = url.getPath();
		int bang = path.indexOf('!');
		String jar = path.substring("file:".length(), bang);
		String elm = path.substring(bang + 2);

		ZipFile z = new ZipFile(jar);
		Enumeration<? extends ZipEntry> ents = z.entries();
		while (ents.hasMoreElements()) {
			ZipEntry ent = ents.nextElement();
			if (ent.getName().startsWith(elm)) {
				add(res, elm, ent.getName());
			}
		}

		z.close();
	}

	private static void add(Set<String> res, String elm, String name) {
		String rest = name.substring(elm.length() + 1);
		int idx;
		if ((idx = rest.indexOf('/')) != -1) {
			res.add(rest.substring(0, idx));
		} else if (rest.length() != 0) {
			res.add(rest);
		}
	}

	public static void fstat(EFile efile, String file_name) throws Pausable {

		// System.err.println("trying entry for "+file_name);

		try {
			ZipEntry ent;
			
			ent = get_entry(file_name + "/");
			if (ent == null) {
				
				ent = get_entry(file_name);
				
				if (ent == null) {
				
					efile.reply_posix_error(Posix.ENOENT);
					return;
				}
			}
			
			// System.err.println("got entry for "+file_name+" : "+ent.toString()+" isdir="+ent.isDirectory());

			long file_size = ent.getSize();
			int file_type = ent.isDirectory() ? EFile.FT_DIRECTORY
					: EFile.FT_REGULAR;

			final int RESULT_SIZE = (1 + (29 * 4));

			ByteBuffer res = ByteBuffer.allocate(RESULT_SIZE);
			res.order(ByteOrder.BIG_ENDIAN);

			res.put(EFile.FILE_RESP_INFO);
			res.putLong(file_size);
			res.putInt(file_type);

			put_time(res, ent.getTime());
			put_time(res, ent.getTime());
			put_time(res, ent.getTime());

			res.putInt(0000400);
			res.putInt(1 /* file_links */);
			res.putInt(0 /* file_major_device */);
			res.putInt(0 /* file_minor_device */);
			res.putInt(file_name.hashCode() /* file_inode */);
			res.putInt(0 /* file_uid */);
			res.putInt(0 /* file_gid */);
			res.putInt(EFile.FA_READ);

			efile.driver_output2(res, null);

		} catch (IOException e) {
			efile.reply_posix_error(IO.exception_to_posix_code(e));
		}

	}

	
	private static void put_time(ByteBuffer res, long time) {
		Calendar c = GregorianCalendar.getInstance();
		c.setTimeInMillis(time);
		
		int year = c.get(Calendar.YEAR);
		res.putInt(year);
		int month = c.get(Calendar.MONTH) - Calendar.JANUARY + 1;
		res.putInt(month);
		int day_of_month = c.get(Calendar.DAY_OF_MONTH);
		res.putInt(day_of_month);
		int hour_of_day = c.get(Calendar.HOUR_OF_DAY);
		res.putInt(hour_of_day);
		int minute_of_hour = c.get(Calendar.MINUTE);
		res.putInt(minute_of_hour);
		int seconds = c.get(Calendar.SECOND);
		res.putInt(seconds);
	}

	static ZipEntry get_entry(String path) throws IOException {
		Enumeration<URL> out = ClassPathResource.class.getClassLoader()
				.getResources(path.substring(EFile.RESOURCE_PREFIX.length()));

		while (out.hasMoreElements()) {
			URL u = out.nextElement();
			ZipEntry result = get_entry(u);
			if (result != null)
				return result;
		}

		return null;
	}

	static ZipEntry get_entry(URL url) throws IOException {

		if (url.getProtocol().equals("jar")) {
			return get_jar_entry(url);
		} else {
			return null;
		}

	}

	private static ZipEntry get_jar_entry(URL url) throws IOException {
		
		// System.err.println("looking at "+url);
		
		String path = url.getPath();
		int bang = path.indexOf('!');
		String jar = path.substring("file:".length(), bang);
		String elm = path.substring(bang + 2);

		ZipFile z = new ZipFile(jar);

		try {
			ZipEntry ze = z.getEntry(elm);
			if (ze != null)
				return ze;
		} finally {
			z.close();
		}

		return null;
	}

}
