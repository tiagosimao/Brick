package com.irenical.brick.test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import junit.framework.Assert;

import com.irenical.brick.BundleInterface;

public class BundleTest {
	
	public static String getFileContents(String fileName,boolean removeWhitespaces) throws IOException{
		InputStream is = DOMBundleTest.class.getClassLoader().getResourceAsStream(DOMBundleTest.class.getPackage().getName().replaceAll("[.]", "/")+"/"+fileName);
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
