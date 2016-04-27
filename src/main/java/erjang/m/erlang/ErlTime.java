package erjang.m.erlang;

import erjang.*;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BinaryOperator;

/**
 * Created by krab on 26/04/16.
 */
public class ErlTime {

    static final EAtom am_native = EAtom.intern("native");
    static final EAtom am_nano_seconds = EAtom.intern("nano_seconds");
    static final EAtom am_micro_seconds = EAtom.intern("micro_seconds");
    static final EAtom am_milli_seconds = EAtom.intern("milli_seconds");
    static final EAtom am_seconds = EAtom.intern("seconds");

    // native time is nano

    @BIF(name = "now")
    static public ETuple3 now() {
        long now = now_unique_micros();
        int micros = (int)(now % 1000000); now /= 1000000;
        int secs   = (int)(now % 1000000); now /= 1000000;
        int megas  = (int)now;

        ETuple3 res = new ETuple3();

        res.elem1 = ERT.box(megas);
        res.elem2 = ERT.box(secs);
        res.elem3 = ERT.box(micros);

        return res;
    }

    final static AtomicLong latest_now = new AtomicLong();
    final static long micros_from_epoch_to_nanotime =
            System.currentTimeMillis() * 1000 - System.nanoTime() / 1000;

    public static long now_raw_micros() {
        return System.nanoTime() / 1000 + micros_from_epoch_to_nanotime;
    }

    static long now_unique_micros() {
		/* now() must fulfill:
		 * - Any return value approximates the current time.
		 * - The return values are strictly increasing (and thus unique).
		 * We ensure the latter by (a) always increasing latest_now,
		 * (b) always returning what we set it to.
		 */
        long micros = now_raw_micros();
        long prev;
        while ((prev = latest_now.get()) < micros) {
            if (latest_now.compareAndSet(prev,micros)) {
                return micros;
            }
        }
        return latest_now.incrementAndGet();
    }


    static long now_monotonic_micros() {
		/* now() must fulfill:
		 * - Any return value approximates the current time.
		 * - The return values are monotonic
		 */
        long micros = now_raw_micros();
        long prev;
        while ((prev = latest_now.get()) <= micros) {
            if (prev == micros || latest_now.compareAndSet(prev,micros)) {
                return micros;
            }
        }
        return latest_now.get();
    }

    @BIF
    public static ENumber monotonic_time()
    {
        return ERT.box(now_monotonic_micros());
    }

    @BIF
    public static EInteger monotonic_time(EObject units)
    {
        return convert_time_unit(monotonic_time(), am_native, units);
    }


    // return current time in micros
    @BIF
    public static EInteger system_time()
    {
        return ERT.box(now_raw_micros());
    }

    @BIF
    public static EInteger system_time(EObject units)
    {
        return convert_time_unit(system_time(), am_native, units);
    }

    @BIF
    public static EInteger convert_time_unit(EObject time0, EObject from, EObject to)
    {
        EInteger time = time0.testInteger();

        if (time == null)
        {
            throw ERT.badarg(time0, from, to);
        }

        int fu = integer_time_unit(from);
        int tu = integer_time_unit(to);

        if (time.is_lt(ESmall.ZERO)) {
            return time.r_multiply(tu).subtract(fu-1).idiv(fu);
        } else {
            return time.r_multiply(tu).idiv(fu);
        }

    }

    private static int integer_time_unit(EObject unit) {
        if (unit == am_nano_seconds) {
            return 1000 * 1000 * 1000;
        } else if (unit == am_micro_seconds || unit == am_native) {
            return 1000 * 1000;
        } else if (unit == am_milli_seconds) {
            return 1000;
        } else if (unit == am_seconds) {
            return 1;
        } else {
            ESmall iu = unit.testSmall();
            if (iu == null) {
                throw new ErlangError(EAtom.intern("bad_time_unit"), ERT.NIL.cons(unit));
            }
            return iu.value;
        }
    }

    static AtomicReference<EInteger> uniqueValue = new AtomicReference<EInteger>(ESmall.ONE);

    @BIF
    public static EInteger unique_integer() {

        return uniqueValue.getAndAccumulate(ESmall.ONE, new BinaryOperator<EInteger>() {
            @Override
            public EInteger apply(EInteger i1, EInteger i2) {
                return (EInteger)i1.add(i2);
            }
        });
    }

    @BIF
    public static EInteger unique_integer(EObject opts) {
        return unique_integer();
    }

}
