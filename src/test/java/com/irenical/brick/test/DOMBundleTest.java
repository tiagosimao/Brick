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

import javax.xml.parsers.ParserConfigurationException;

import org.junit.BeforeClass;
import org.junit.Test;
import org.xml.sax.SAXException;

import com.irenical.brick.xml.DOMBundle;


public class DOMBundleTest extends BundleTest {
	
	private static String simpleXML;
	
	private static DOMBundle bundle;
	
	@BeforeClass
	public static void setupClass() throws IOException, ParserConfigurationException, SAXException{
		simpleXML = getFileContents("plant_catalog.xml",false);
		bundle = new DOMBundle(simpleXML);
	}
	
	@Test
	public void testBundle(){
		testStringBundle(bundle);
	}
	
	@Test
	public void testBundleToString(){
		try{
			testStringBundle(new DOMBundle(bundle.toString()));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	

}
