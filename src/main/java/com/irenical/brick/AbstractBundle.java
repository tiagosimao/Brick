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
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class AbstractBundle<KEY_CLASS> implements BundleInterface<KEY_CLASS> {

	private static final String convertToString(Object from) {
		return (((from instanceof String) || (from == null)) ? (String) from : from.toString());
	}

	private static final Number convertToNumber(Object from) {
		return (((from instanceof Number) || (from == null)) ? (Number) from : Double.valueOf(from.toString()));
	}

	private static final Double convertToDouble(Object from) {
		return ((from instanceof Double) || (from == null)) ? ((Double) from) : Double.valueOf(from.toString());
	}

	private static final Long convertToLong(Object from) {
		return ((from instanceof Long) || (from == null)) ? ((Long) from) : Long.valueOf(from.toString());
	}

	private static final Float convertToFloat(Object from) {
		return ((from instanceof Float) || (from == null)) ? ((Float) from) : Float.valueOf(from.toString());
	}

	private static final Integer convertToInteger(Object from) {
		return ((from instanceof Integer) || (from == null)) ? ((Integer) from) : Integer.valueOf(from.toString());
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

	private class AbstractBundleEntry implements Map.Entry<KEY_CLASS, Object> {

		private final KEY_CLASS key;

		private Object value;

		AbstractBundleEntry(KEY_CLASS key, Object value) {
			this.key = key;
			this.value = value;
		}

		@Override
		public KEY_CLASS getKey() {
			return key;
		}

		@Override
		public Object getValue() {
			return value;
		}

		@Override
		public Object setValue(Object value) {
			Object result = this.value;
			this.value = value;
			return result;
		}

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
	public final Iterable<Object> getObjects(KEY_CLASS key) {
		Iterable<Object> result = null;
		Object got = getObject(key);
		if (got instanceof Iterable<?>) {
			result = (Iterable<Object>) got;
		} else if (got != null) {
			result = Collections.singletonList(got);
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <BUNDLE_TYPE extends BundleInterface<KEY_CLASS>> BUNDLE_TYPE getBundle(KEY_CLASS key) {
		return (BUNDLE_TYPE) getObject(key);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <BUNDLE_TYPE extends BundleInterface<KEY_CLASS>> Iterable<BUNDLE_TYPE> getBundles(KEY_CLASS key) {
		Object value = getObject(key);
		if (value instanceof Iterable) {
			return (Iterable<BUNDLE_TYPE>) value;
		} else {
			return Collections.singletonList((BUNDLE_TYPE) value);
		}
	}

	@Override
	public boolean equals(Object object) {
		boolean result = object instanceof BundleInterface<?>;
		if (result) {
			BundleInterface<?> bundle = (BundleInterface<?>) object;
			Set<KEY_CLASS> keys = getKeys();
			Set<?> targetKeys = bundle.getKeys();
			if ((keys == null && targetKeys == null) || (keys != null && keys.equals(targetKeys))) {
				@SuppressWarnings("unchecked")
				BundleInterface<KEY_CLASS> targetBundle = (BundleInterface<KEY_CLASS>) bundle;
				for (KEY_CLASS key : getKeys()) {
					Object value = getObject(key);
					Object targetValue = targetBundle.getObject(key);
					result &= (value == null && targetValue == null) || (value != null && value.equals(targetValue));
				}
			}
		}
		return result;
	}

	@Override
	public int hashCode() {
		int result = 0;
		for (KEY_CLASS key : getKeys()) {
			result += key.hashCode();
			Object value = getObject(key);
			if (value != null) {
				result += value.hashCode();
			}
		}
		return result;
	}

	@Override
	public void clear() {
		throw new RuntimeException("Not implemented");
	}

	@Override
	public boolean containsKey(Object key) {
		return getKeys().contains(key);
	}

	@Override
	public boolean containsValue(Object value) {
		throw new RuntimeException("Not implemented");
	}

	@Override
	public Set<Map.Entry<KEY_CLASS, Object>> entrySet() {
		Set<Map.Entry<KEY_CLASS, Object>> result = new HashSet<Map.Entry<KEY_CLASS, Object>>();
		for (KEY_CLASS key : getKeys()) {
			result.add(new AbstractBundleEntry(key, getObject(key)));
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object get(Object key) {
		return getObject((KEY_CLASS) key);
	}

	@Override
	public boolean isEmpty() {
		return getKeys().isEmpty();
	}

	@Override
	public Set<KEY_CLASS> keySet() {
		return getKeys();
	}

	public Object put(KEY_CLASS paramK, Object paramV) {
		throw new RuntimeException("Not implemented");
	}

	@Override
	public void putAll(Map<? extends KEY_CLASS, ? extends Object> paramMap) {
		throw new RuntimeException("Not implemented");
	}

	@Override
	public Object remove(Object paramObject) {
		throw new RuntimeException("Not implemented");
	}

	@Override
	public int size() {
		return getKeys().size();
	}

	@Override
	public Collection<Object> values() {
		throw new RuntimeException("Not implemented");
	}

}
