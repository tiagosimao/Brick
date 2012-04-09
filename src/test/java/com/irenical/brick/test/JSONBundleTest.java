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
