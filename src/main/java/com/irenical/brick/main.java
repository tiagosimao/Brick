package com.irenical.brick;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.xml.parsers.ParserConfigurationException;

import org.json.JSONException;
import org.xml.sax.SAXException;

import com.irenical.brick.json.JSONBundle;
import com.irenical.brick.xml.DOMBundle;

public class main {

	/**
	 * @param args
	 * @throws IOException 
	 * @throws SAXException 
	 * @throws ParserConfigurationException 
	 * @throws JSONException 
	 */
	public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException, JSONException {
		
		String simpleXML = getFileContents("plant_catalog.xml",false);
		String json = getFileContents("plant_catalog.json", false);
		
		BundleInterface<String> domBundle = new DOMBundle(simpleXML);
		BundleInterface<String> jsonBundle = new JSONBundle(json);
		
		BundleInterface<String> tester = new DOMBundle(jsonBundle);
		
		System.out.println(tester.toString());
	}
	
	public static String getFileContents(String fileName,boolean removeWhitespaces) throws IOException{
		InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(fileName);
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
}
