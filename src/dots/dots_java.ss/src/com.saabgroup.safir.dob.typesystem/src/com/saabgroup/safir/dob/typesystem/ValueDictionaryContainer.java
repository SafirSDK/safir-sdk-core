//TODO header
package com.saabgroup.safir.dob.typesystem;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class ValueDictionaryContainer<K, C extends ValueContainer<V>, V>
    extends DictionaryContainer<K,C>
{
    public ValueDictionaryContainer(final Class containerClass) {
        super();
        m_containerClass = (Class<C>)containerClass;
    }

    public C putVal(K key, V value) {
        m_isChanged=true;
        C container;
        try {
            container = m_containerClass.newInstance();
        }
        catch (InstantiationException | IllegalAccessException e) {
            throw new SoftwareViolationException("Internal error in ValueDictionaryContainer: " +
                                                 "Failed to instantiate container.");
        }
        container.setVal(value);

        return m_values.put(key, container);
    }

    private final Class<C> m_containerClass;
}
