//TODO header
package com.saabgroup.safir.dob.typesystem;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class BinaryDictionaryContainer<K>
    extends DictionaryContainer<K,BinaryContainer>
{
    public BinaryContainer putVal(K key, byte[] value) {
        m_isChanged=true;
        BinaryContainer container = new BinaryContainer();
        container.setVal(value);

        return m_values.put(key, container);
    }
}
