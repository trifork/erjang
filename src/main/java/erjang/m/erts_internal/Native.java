package erjang.m.erts_internal;

import erjang.*;
import erjang.m.erlang.ErlProc;
import kilim.Pausable;

public class Native extends ENative {

    static final EAtom am_check_process_code = EAtom.intern("check_process_code");
    static final EAtom am_garbage_collect = EAtom.intern("garbage_collect");

    @BIF
    public static EAtom is_system_process(EObject pid) {
        return ERT.FALSE;
    }

    private static boolean check_process_code(EModule mod)
    {
        Class mc = mod.getClass();
        String className = mc.getName();
        for (Class c : sm.getClassContext()) {
            if (className.equals(c.getName()) && mc != c) {
               return true;
            }
        }

        return false;
    }


    /*
    -spec request_system_task(Pid, Prio, Request) -> 'ok' when
      Prio :: 'max' | 'high' | 'normal' | 'low',
      Request :: {'garbage_collect', term()}
	       | {'check_process_code', term(), module(), boolean()},
      Pid :: pid().
     */
    @BIF
    public static EAtom request_system_task(final EProc self, EObject pid0, EObject prio0, EObject request0)
        throws Pausable
    {

        EInternalPID pid = pid0.testInternalPID();
        EAtom prio = prio0.testAtom();
        ETuple4 cpc = ETuple4.cast(request0);
        ETuple2 gc = ETuple2.cast(request0);

        if (pid == null || prio == null || (cpc==null && gc==null))
            throw ERT.badarg(pid0, prio0, request0);

        EObject ref;
        EAtom module, allow_gc;
        if (cpc != null
                && cpc.elem1 == am_check_process_code
                && (ref = cpc.elem2) != null
                && (module = cpc.elem3.testAtom()) != null
                && (allow_gc = cpc.elem4.testBoolean()) != null) {

            final EModule mod = EModuleManager.get_loaded_module(module);
            final EObject final_ref = ref;

            if (mod == null || !pid.is_alive_dirtyread()) {
                self.mbox_send(ETuple3.make_tuple(am_check_process_code, ref, ERT.FALSE));

            } else if (pid0 == self.self_handle()) {
                self.mbox_send(ETuple3.make_tuple(
                                am_check_process_code,
                                ref,
                                ERT.box(check_process_code(mod))));
            } else {
                Runnable runner =
                        new Runnable() {
                            @Override
                            public void run() {
                                self.mbox()
                                        .putb(ETuple3.make_tuple(
                                        am_check_process_code,
                                        final_ref,
                                        ERT.box(check_process_code(mod))));
                            }
                        };

                pid.task().runOnStack(runner);
            }

        } else if (gc != null
                && gc.elem1 == am_garbage_collect
                && (ref = gc.elem2) != null) {
            // We cannot force GC of a single process...
            self.mbox_send(ETuple3.make_tuple(am_garbage_collect, ref, ERT.TRUE));
        } else {
            throw ERT.badarg(pid0, prio0, request0);
        }

        return ERT.am_ok;
    }

    static SM sm = new SM();
    static class SM extends SecurityManager {
        @Override
        public Class[] getClassContext() {
            return super.getClassContext();
        }
    }
}
