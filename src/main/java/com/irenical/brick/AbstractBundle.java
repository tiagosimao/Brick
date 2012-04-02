/*
	This file is part of Brick.

    Brick is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Brick is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Brick.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.irenical.brick;

import java.text.Format;
import java.text.ParseException;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

public abstract class AbstractBundle<KEY_CLASS> implements BundleInterface<KEY_CLASS> {
	
	private static final String convertToString(Object from) {
		return (((from instanceof String) || (from == null)) ? (String) from : from.toString());
	}

	private static final Number convertToNumber(Object from) {
		return (((from instanceof Number) || (from == null)) ? (Number) from : Double.valueOf(Double.parseDouble(from.toString())));
	}

	private static final Double convertToDouble(Object from) {
		return Double.valueOf(((from instanceof Double) || (from == null)) ? ((Double) from).doubleValue() : Double.parseDouble(from.toString()));
	}

	private static final Long convertToLong(Object from) {
		return Long.valueOf(((from instanceof Long) || (from == null)) ? ((Long) from).longValue() : Long.parseLong(from.toString()));
	}

	private static final Float convertToFloat(Object from) {
		return Float.valueOf(((from instanceof Float) || (from == null)) ? ((Float) from).floatValue() : Float.parseFloat(from.toString()));
	}

	private static final Integer convertToInteger(Object from) {
		return Integer.valueOf(((from instanceof Integer) || (from == null)) ? ((Integer) from).intValue() : Integer.parseInt(from.toString()));
	}

	private static final Date convertToDate(Object from) {
		return (((from instanceof Date) || (from == null)) ? (Date) from : null);
	}

	private static final Boolean convertToBoolean(Object from) {
		if (from != null) {
			if (Boolean.TRUE.toString().equals(from.toString())) {
				return Boolean.TRUE;
			}
			if (Boolean.FALSE.toString().equals(from.toString())) {
				return Boolean.FALSE;
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <RETURNS> RETURNS coallesce(KEY_CLASS[] keys) {
		Object result = null;
		if (keys != null) {
			for (KEY_CLASS key : keys) {
				result = getObject(key);
				if (result != null) {
					break;
				}
			}
		}
		return (RETURNS) result;
	}

	protected Number getNumber(KEY_CLASS key) {
		return convertToNumber(getObject(key));
	}

	@Override
	public String getString(KEY_CLASS key) {
		return convertToString(getObject(key));
	}

	@Override
	public Iterable<String> getStrings(KEY_CLASS key) {
		List<String> result = new LinkedList<String>();
		for (Object o : getObjects(key)) {
			result.add(convertToString(o));
		}
		return result;
	}

	@Override
	public Double getDouble(KEY_CLASS key) {
		return convertToDouble(getObject(key));
	}

	@Override
	public Iterable<Double> getDoubles(KEY_CLASS key) {
		List<Double> result = new LinkedList<Double>();
		for (Object o : getObjects(key)) {
			result.add(convertToDouble(o));
		}
		return result;
	}

	@Override
	public Long getLong(KEY_CLASS key) {
		return convertToLong(getObject(key));
	}

	@Override
	public Iterable<Long> getLongs(KEY_CLASS key) {
		List<Long> result = new LinkedList<Long>();
		for (Object o : getObjects(key)) {
			result.add(convertToLong(o));
		}
		return result;
	}

	@Override
	public Float getFloat(KEY_CLASS key) {
		return convertToFloat(getObject(key));
	}

	@Override
	public Iterable<Float> getFloats(KEY_CLASS key) {
		List<Float> result = new LinkedList<Float>();
		for (Object o : getObjects(key)) {
			result.add(convertToFloat(o));
		}
		return result;
	}

	@Override
	public Integer getInteger(KEY_CLASS key) {
		return convertToInteger(getObject(key));
	}

	@Override
	public Iterable<Integer> getIntegers(KEY_CLASS key) {
		List<Integer> result = new LinkedList<Integer>();
		for (Object o : getObjects(key)) {
			result.add(convertToInteger(o));
		}
		return result;
	}

	@Override
	public Boolean getBoolean(KEY_CLASS key) {
		return convertToBoolean(getObject(key));
	}

	@Override
	public Iterable<Boolean> getBooleans(KEY_CLASS key) {
		List<Boolean> result = new LinkedList<Boolean>();
		for (Object o : getObjects(key)) {
			result.add(convertToBoolean(o));
		}
		return result;
	}

	@Override
	public Date getDate(KEY_CLASS key) {
		return convertToDate(getObject(key));
	}

	@Override
	public Iterable<Date> getDates(KEY_CLASS key) {
		List<Date> result = new LinkedList<Date>();
		for (Object o : getObjects(key)) {
			result.add(convertToDate(o));
		}
		return result;
	}

	@Override
	public String format(KEY_CLASS key, Format formatter) {
		Object result = getObject(key);
		return ((formatter == null) ? result.toString() : (result == null) ? null : formatter.format(result));
	}

	@Override
	public Object parse(KEY_CLASS key, Format parser) throws ParseException {
		Object result = getObject(key);
		return ((parser == null) ? result : (result == null) ? null : parser.parseObject(result.toString()));
	}

	@SuppressWarnings("unchecked")
	@Override
	public <BUNDLE_TYPE extends BundleInterface<KEY_CLASS>> BUNDLE_TYPE getBundle(KEY_CLASS key) {
		return (BUNDLE_TYPE) createBundle(getObject(key));
	}

	@SuppressWarnings("unchecked")
	@Override
	public <BUNDLE_TYPE extends BundleInterface<KEY_CLASS>> Iterable<BUNDLE_TYPE> getBundles(KEY_CLASS key) {
		List<BUNDLE_TYPE> result = null;
		Object value = getObject(key);
		if (value instanceof Iterable) {
			result = new LinkedList<BUNDLE_TYPE>();
			for (Object each : ((Iterable<?>) value)) {
				result.add((BUNDLE_TYPE) createBundle(each));
			}
		} else {
			result = (List<BUNDLE_TYPE>) Collections.singletonList(createBundle(value));
		}
		return result;
	}

	protected abstract BundleInterface<KEY_CLASS> createBundle(Object paramObject);

}
