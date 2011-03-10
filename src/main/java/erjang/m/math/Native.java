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
			return boxIfValid(Math.tan(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber asin(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.asin(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static ENumber acos(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.acos(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble atan(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.atan(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble sinh(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.sinh(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble cosh(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.cosh(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}
	
	@BIF
	public static EDouble tanh(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.tanh(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}

	@BIF
	public static EDouble exp(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.exp(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}

	@BIF
	public static EDouble log(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.log(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}

	@BIF
	public static ENumber log10(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.log10(num.doubleValue()), val);
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
		return boxIfValid(Math.sqrt(val.value), val);
	}

	@BIF
	public static EDouble sqrt(EObject val)
	{
		ENumber num;
		if ((num = val.testNumber()) != null) {
			return boxIfValid(Math.sqrt(num.doubleValue()), val);
		}
		throw ERT.badarg(val);
	}

	public static EDouble boxIfValid(double value, EObject arg) {
		if (Double.isNaN(value) || Double.isInfinite(value)) throw ERT.badarg(arg);
		else return new EDouble(value);
	}

}
