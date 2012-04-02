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
import java.util.Date;
import java.util.Set;

public abstract interface BundleInterface<KEY_CLASS>
{
  public abstract <RETURNS> RETURNS coallesce(KEY_CLASS[] paramArrayOfKEY_CLASS);

  public abstract Set<KEY_CLASS> getKeys();

  public abstract Object getObject(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Object> getObjects(KEY_CLASS paramKEY_CLASS);

  public abstract String getString(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<String> getStrings(KEY_CLASS paramKEY_CLASS);

  public abstract Double getDouble(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Double> getDoubles(KEY_CLASS paramKEY_CLASS);

  public abstract Long getLong(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Long> getLongs(KEY_CLASS paramKEY_CLASS);

  public abstract Float getFloat(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Float> getFloats(KEY_CLASS paramKEY_CLASS);

  public abstract Integer getInteger(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Integer> getIntegers(KEY_CLASS paramKEY_CLASS);

  public abstract Boolean getBoolean(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Boolean> getBooleans(KEY_CLASS paramKEY_CLASS);

  public abstract Date getDate(KEY_CLASS paramKEY_CLASS);

  public abstract Iterable<Date> getDates(KEY_CLASS paramKEY_CLASS);

  public abstract String format(KEY_CLASS paramKEY_CLASS, Format paramFormat);

  public abstract Object parse(KEY_CLASS paramKEY_CLASS, Format paramFormat)
    throws ParseException;

  public abstract <BUNDLE_TYPE extends BundleInterface<KEY_CLASS>> BUNDLE_TYPE getBundle(KEY_CLASS paramKEY_CLASS);

  public abstract <BUNDLE_TYPE extends BundleInterface<KEY_CLASS>> Iterable<BUNDLE_TYPE> getBundles(KEY_CLASS paramKEY_CLASS);
}
