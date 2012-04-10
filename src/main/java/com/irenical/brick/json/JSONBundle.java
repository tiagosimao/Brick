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

package com.irenical.brick.json;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.irenical.brick.AbstractBundle;
import com.irenical.brick.BundleInterface;

public class JSONBundle extends AbstractBundle<String> {

	private final JSONObject json;

	public JSONBundle(BundleInterface<String> wrapped) {
		super(wrapped);
		this.json = null;
	}

	public JSONBundle(String value) throws JSONException {
		super(null);
		this.json = new JSONObject(value);
	}

	public JSONBundle(JSONObject value) {
		super(null);
		this.json = value;
	}

	@Override
	public Set<String> getKeys() {
		if (json != null) {
			String[] names = JSONObject.getNames(json);
			return names == null ? null : new HashSet<String>(Arrays.asList(names));
		} else {
			return super.getKeys();
		}
	}

	private Object innerGet(Object value) {
		if (value instanceof JSONObject) {
			return new JSONBundle((JSONObject) value);
		} else if (value instanceof JSONBundle) {
			return (JSONBundle) value;
		}
		return value;
	}

	@Override
	public Object getObject(String key) {
		if (json != null) {
			List<Object> resultSeveral = null;
			Object resultOne = null;
			JSONArray array = json.optJSONArray(key);
			if (array != null) {
				for (int i = 0; i < array.length(); ++i) {
					Object item = innerGet(array.opt(i));
					if (i == 0) {
						resultOne = item;
					} else if (i == 1) {
						resultSeveral = new LinkedList<Object>();
						resultSeveral.add(resultOne);
						resultSeveral.add(item);
					} else {
						resultSeveral.add(item);
					}
				}
			} else {
				resultOne = innerGet(json.opt(key));
			}
			return resultSeveral != null ? resultSeveral : resultOne;
		} else {
			return super.getObject(key);
		}
	}

	@Override
	public String toString() {
		String result = null;
		if (json != null) {
			result = json.toString();
		} else if (wrapped != null) {
			result = new JSONObject(wrapped).toString();
		}
		return result;
	}

}
