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
package com.irenical.brick.test;

import java.io.IOException;

import org.json.JSONException;
import org.junit.BeforeClass;
import org.junit.Test;

import com.irenical.brick.json.JSONBundle;


public class JSONBundleTest extends BundleTest {
	
	private static String simpleJSON;
	
	private static JSONBundle bundle;
	
	@BeforeClass
	public static void setupClass() throws IOException, JSONException{
		simpleJSON = getFileContents("plant_catalog.json",true);
		bundle = new JSONBundle(simpleJSON);
	}
	
	@Test
	public void testBundle(){
		testStringBundle(bundle);
	}
	
	@Test
	public void testBundleToString() throws JSONException{
		testStringBundle(new JSONBundle(bundle.toString()));
	}
	

}
