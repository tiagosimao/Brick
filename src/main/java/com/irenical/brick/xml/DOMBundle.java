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

package com.irenical.brick.xml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.hamcrest.core.IsInstanceOf;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.irenical.brick.AbstractBundle;
import com.irenical.brick.BundleInterface;

public class DOMBundle extends AbstractBundle<String> {

	private final Node node;
	
	public DOMBundle(BundleInterface<String> wrapped){
		super(wrapped);
		this.node=null;
	}

	public DOMBundle(String xml) throws IOException, ParserConfigurationException, SAXException {
		super(null);
		ByteArrayInputStream is = new ByteArrayInputStream(xml.getBytes());
		node = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
	}

	public DOMBundle(Node node) {
		super(null);
		this.node = node;
	}

	@Override
	public Set<String> getKeys() {
		Set<String> result = null;
		if(node!=null){
			result = new HashSet<String>();
			NodeList children = node.getChildNodes();
			for (int i = 0; i < children.getLength(); ++i) {
				Node child = children.item(i);
				if (Node.ELEMENT_NODE == child.getNodeType()) {
					result.add(child.getNodeName());
				}
			}
		} else {
			result = super.getKeys();
		}
		return result;
	}

	@Override
	public Object getObject(String key) {
		if(node!=null){
			List<Object> resultSeveral = null;
			Object resultOne = null;
			int found = 0;
			NodeList children = node.getChildNodes();
			for (int i = 0; i < children.getLength(); ++i) {
				Node child = children.item(i);
				if (key.equals(child.getNodeName())) {
					Object got = findValue(child);
					if (got instanceof Node) {
						got = new DOMBundle((Node)got);
					}
					if(found==0){
						resultOne = got;
					} else if(found == 1 ){
						resultSeveral = new LinkedList<Object>();
						resultSeveral.add(resultOne);
						resultSeveral.add(got);
					} else {
						resultSeveral.add(got);
					}
					++found;
				}
			}
			return resultSeveral != null ? resultSeveral : resultOne;
		} else {
			return super.getObject(key);
		}
	}

	private static Object findValue(Node node) {
		Object result = null;
		if (node != null) {
			if (Node.ELEMENT_NODE == node.getNodeType()) {
				NodeList children = node.getChildNodes();
				if (children != null) {
					if (children.getLength() > 1) {
						result = node;
					} else if (children.getLength() == 1) {
						node = children.item(0);
					} else {
						node = null;
					}
				}
			}
			if (result == null && node != null) {
				if (Node.TEXT_NODE == node.getNodeType() || Node.CDATA_SECTION_NODE == node.getNodeType()) {
					result = node.getNodeValue();
				}
			}
		}
		return result;
	}
	
	@Override
	public String toString() {
		String result = null;
		try{
			Node node = null;
			if(this.node!=null){
				node = this.node; 
			} else if(wrapped!=null){
				node = createNode(wrapped);
			}
			if(node!=null){
				TransformerFactory transFactory = TransformerFactory.newInstance();
				Transformer transformer = transFactory.newTransformer();
				StringWriter buffer = new StringWriter();
				transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
				transformer.transform(new DOMSource(node),new StreamResult(buffer));
				result = buffer.toString();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

	// TODO not finished
	@SuppressWarnings("unchecked")
	private Node createNode(BundleInterface<String> wrapped) throws ParserConfigurationException {
		
//		DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
//		DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
//		
//		// root elements
//		Document doc = docBuilder.newDocument();
//
//		for(String key : wrapped.getKeys()){			
//			
//			System.out.println(key);
//			
//			Element rootElement = doc.createElement(key);
//			doc.appendChild(rootElement);
//			
//			Object object = wrapped.getObject(key);
//			
//			System.out.println(object.getClass());
//			
//			if(object instanceof BundleInterface){
//				rootElement.appendChild(createNode((BundleInterface<String>)object));
//			}
//			else if(object instanceof Iterable<?>){
//				for(Object bundle : (Iterable<?>) object){
//					rootElement.appendChild(createNode((BundleInterface<String>) bundle));
//				}
//			}
//			else if(object instanceof String){
//				Element element = doc.createElement(key);
//				element.setNodeValue((String) object);
//				rootElement.appendChild(element);
//			}
//			
//			this.printDoc(doc);
//		}
//		
//		return doc;
		
		return null;
	}

	// TODO delete after createNode is complete
	private void printDoc(Document doc){
		try
	    {
	       DOMSource domSource = new DOMSource(doc);
	       StringWriter writer = new StringWriter();
	       StreamResult result = new StreamResult(writer);
	       TransformerFactory tf = TransformerFactory.newInstance();
	       Transformer transformer = tf.newTransformer();
	       transformer.transform(domSource, result);
	       System.out.println(writer.toString());
	    }
	    catch(TransformerException ex)
	    {
	       ex.printStackTrace();
	    }
	}
}
