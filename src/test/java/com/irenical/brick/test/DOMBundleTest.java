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
			System.out.println(bundle.toString());
			testStringBundle(new DOMBundle(bundle.toString()));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	

}
