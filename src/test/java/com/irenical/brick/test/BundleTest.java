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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import junit.framework.Assert;

import com.irenical.brick.BundleInterface;

public class BundleTest {
	
	public static String getFileContents(String fileName,boolean removeWhitespaces) throws IOException{
		InputStream is = DOMBundleTest.class.getClassLoader().getResourceAsStream(fileName);
		BufferedReader reader = new BufferedReader(new InputStreamReader(is));
		StringBuilder sb = new StringBuilder();
		String line = null;
		while((line = reader.readLine()) != null){
			sb.append(line);
		}
		String result = sb.toString();
		if(removeWhitespaces){
			result = result.replaceAll("\\s", "");
		}
		return result;
	}
	
	public static void testStringBundle(BundleInterface<String> bundle){
		BundleInterface<String> root = bundle.getBundle("CATALOG");
		Assert.assertNotNull(root);
		Assert.assertTrue(root instanceof BundleInterface<?>);
		Iterable<BundleInterface<String>> plants = root.getBundles("PLANT");
		Assert.assertNotNull(plants);
		int count = 0;
		for(BundleInterface<String> plant : plants){
			Assert.assertNotNull(plant.get("COMMON"));
			Assert.assertNotNull(plant.get("BOTANICAL"));
			Assert.assertNotNull(plant.get("LIGHT"));
			Assert.assertNotNull(plant.get("PRICE"));
			Assert.assertNotNull(plant.get("ZONE"));
			Assert.assertNotNull(plant.getInteger("AVAILABILITY"));
			++count;
		}
		Assert.assertEquals(36, count);
	}

}
