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
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.SAXException;

import com.irenical.brick.AbstractBundle;

public class DOMBundle extends AbstractBundle<String> {

	private final Node node;

	public DOMBundle(String xml) throws IOException, ParserConfigurationException, SAXException {
		ByteArrayInputStream is = new ByteArrayInputStream(xml.getBytes());
		node = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
	}

	public DOMBundle(Node node) {
		this.node = node;
	}

	@Override
	public Set<String> getKeys() {
		Set<String> result = new HashSet<String>();
		NodeList children = node.getChildNodes();
		for (int i = 0; i < children.getLength(); ++i) {
			Node child = children.item(i);
			if (Node.ELEMENT_NODE == child.getNodeType()) {
				result.add(child.getNodeName());
			}
		}
		return result;
	}

	@Override
	public Object getObject(String key) {
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
			TransformerFactory transFactory = TransformerFactory.newInstance();
			Transformer transformer = transFactory.newTransformer();
			StringWriter buffer = new StringWriter();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			transformer.transform(new DOMSource(node),new StreamResult(buffer));
			result = buffer.toString();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
//		Document document = (node instanceof Document) ? ((Document)node) : node.getOwnerDocument();
//		DOMImplementationLS domImplLS = (DOMImplementationLS) document.getImplementation();
//		LSSerializer serializer = domImplLS.createLSSerializer();
//		return serializer.writeToString(node);
	}

}
