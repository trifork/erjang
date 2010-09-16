/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
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


package erjang.m.math;

import erjang.BIF;
import erjang.EDouble;
import erjang.ENative;
import erjang.ENumber;
import erjang.EObject;
import erjang.ERT;

/**
 * 
 */
public class Native extends ENative {

	
	@BIF
	public static ENumber log10(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.log10(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber sin(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.sin(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber cos(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.cos(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber tan(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.tan(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber asin(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.asin(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber acos(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.acos(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble atan(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.atan(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble sinh(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.sinh(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble cosh(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.cosh(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble tanh(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.tanh(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}

	@BIF
	public static EDouble exp(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.exp(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}

	@BIF
	public static EDouble log(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.log(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}

	@BIF
	public static EDouble atan2(EObject val1, EObject val2)
	{
		ENumber num1, num2;
		if ((num1 = val1.testNumber()) != null 
			&& (num2 = val2.testNumber()) != null) {
			return new EDouble(Math.atan2(num1.doubleValue(), num2.doubleValue()));
		}
		throw ERT.badarg(val1, val2);
	}

	@BIF
	public static EDouble pi()
	{
		return new EDouble(Math.PI);
	}

	@BIF
	public static EDouble sqrt(EDouble val)
	{
		return new EDouble(Math.sqrt(val.value));
	}

	@BIF
	public static EDouble sqrt(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return new EDouble(Math.sqrt(num.doubleValue()));
		}
		throw ERT.badarg(val);
	}


}
