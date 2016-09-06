//TODO header
package com.saabgroup.safir.dob.typesystem;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class StringDictionaryContainer<K>
    extends DictionaryContainer<K,StringContainer>
{
    public StringContainer putVal(K key, String value) {
        m_isChanged=true;
        StringContainer container = new StringContainer();
        container.setVal(value);

        return m_values.put(key, container);
    }
}
