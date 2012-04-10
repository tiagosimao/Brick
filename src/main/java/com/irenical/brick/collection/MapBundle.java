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

package com.irenical.brick.collection;

import java.util.Map;
import java.util.Set;

import com.irenical.brick.AbstractBundle;
import com.irenical.brick.BundleInterface;

public class MapBundle<KEY_CLASS> extends AbstractBundle<KEY_CLASS> {
	
	private final Map<KEY_CLASS, ?> data;
	
	public MapBundle(BundleInterface<KEY_CLASS> wrapped) {
		super(wrapped);
		this.data = wrapped;
	}

	public MapBundle(Map<KEY_CLASS, ?> data) {
		super(null);
		this.data = data;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Object getObject(KEY_CLASS key) {
		Iterable<Object> resultSeveral = null;
		Object resultOne = null;
		Object got = this.data.get(key);
		if(got instanceof Iterable<?>){
			resultSeveral = (Iterable<Object>) got;
		} else if (got != null){
			resultOne = got;
		}
		return resultSeveral == null ? resultOne : resultSeveral;
	}

	@Override
	public Set<KEY_CLASS> getKeys() {
		return this.data.keySet();
	}

}
