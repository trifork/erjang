package erjang.m.erts_internal;

import erjang.BIF;
import erjang.EAtom;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;

public class Native extends ENative {

    @BIF
    public static EAtom is_system_process(EObject pid) {
        return ERT.FALSE;
    }
    
}
