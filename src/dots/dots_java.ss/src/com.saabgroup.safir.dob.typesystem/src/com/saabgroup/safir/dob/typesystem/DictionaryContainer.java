package com.saabgroup.safir.dob.typesystem;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class DictionaryContainer<K, V extends ContainerBase>
extends ContainerBase
implements java.util.Map<K, V> {

    public DictionaryContainer() {
        super();
    }

    @Override
    public boolean isNull() {
        return false;
    }

    @Override
    public void setNull() {
        throw new SoftwareViolationException("Dictionaries cannot be null!");
    }

    @Override
    public boolean isChanged() {
        if (m_isChanged)
            return m_isChanged;

        for (V val : m_values.values()) {
            if (val.isChanged())
                return true;
        }

        return false;
    }

    @Override
    public void setChanged(boolean changed) {
        m_isChanged = changed;
        for (V val : m_values.values()) {
            val.setChanged(changed);
        }
    }

    @Override
    public void clear() {
        m_isChanged = true;
        m_values.clear();
    }

    @Override
    public boolean containsKey(java.lang.Object key) {
        return m_values.containsKey(key);
    }

    @Override
    public boolean containsValue(java.lang.Object value) {
        return m_values.containsValue(value);
    }

    @Override
    public Set<java.util.Map.Entry<K, V>> entrySet() {
        return m_values.entrySet();
    }

    @Override
    public V get(java.lang.Object key) {
        return m_values.get(key);
    }

    @Override
    public boolean isEmpty() {
        return m_values.isEmpty();
    }

    @Override
    public Set<K> keySet() {
        return m_values.keySet();
    }

    @Override
    public V put(K key, V value) {
        m_isChanged=true;
        value.setChanged(true);
        return m_values.put(key, value);
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        m_isChanged=true;
        for (V v : m.values()) {
            v.setChanged(true);
        }
        m_values.putAll(m);
    }

    @Override
    public V remove(java.lang.Object key) {
        m_isChanged=true;
        return m_values.remove(key);
    }

    @Override
    public int size() {
        return m_values.size();
    }

    @Override
    public Collection<V> values() {
        return m_values.values();
    }

    @Override
    @SuppressWarnings("unchecked")
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        DictionaryContainer that = (DictionaryContainer)other;
        m_values = that.m_values;
    }

    protected java.util.TreeMap<K, V> m_values = new java.util.TreeMap<K, V>();
}
