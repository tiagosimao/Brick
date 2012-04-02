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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.irenical.brick.AbstractBundle;

public class MapBundle<KEY_CLASS> extends AbstractBundle<KEY_CLASS> {
	private final Map<KEY_CLASS, ?> data;

	public MapBundle(Map<KEY_CLASS, ?> data) {
		this.data = data;
	}
	
	@Override
	public Object getObject(KEY_CLASS key) {
		return this.data.get(key);
	}

	@Override
	public Iterable<Object> getObjects(KEY_CLASS key) {
		List<Object> result = new LinkedList<Object>();
		Object got = this.data.get(key);
		if (got instanceof Iterable<?>) {
			Iterator<?> it = ((Iterable<?>) got).iterator();
			while (it.hasNext()) {
				result.add(it.next());
			}
		}
		return result;
	}

	@Override
	public Set<KEY_CLASS> getKeys() {
		return this.data.keySet();
	}

	@SuppressWarnings("unchecked")
	@Override
	protected AbstractBundle<KEY_CLASS> createBundle(Object value) {
		return ((value instanceof Map<?,?>) ? new MapBundle<KEY_CLASS>((Map<KEY_CLASS,?>) value) : null);
	}
	
}
