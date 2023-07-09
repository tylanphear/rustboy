pub struct Mem<const SIZE: usize>(Box<[u8; SIZE]>);

impl<const SIZE: usize> std::fmt::Debug for Mem<SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Mem")
            .field("size", &self.len())
            .field("data...", &&self.0[..std::cmp::min(16, SIZE)])
            .finish()
    }
}

impl<const SIZE: usize> Default for Mem<SIZE> {
    fn default() -> Self {
        Mem(Box::new([0; SIZE]))
    }
}

impl<const SIZE: usize> Mem<SIZE> {
    #[inline]
    pub fn clear(&mut self) {
        *self.0.as_mut() = [0; SIZE];
    }

    #[inline]
    pub fn len(&self) -> usize {
        SIZE
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    #[inline]
    pub fn slice(&self, addr: usize, n: usize) -> &[u8] {
        &self.0[addr..addr + n]
    }

    #[inline]
    pub fn safe_slice(&self, addr: usize, n: usize) -> &[u8] {
        let safe_len = std::cmp::min(self.len() - addr, n);
        self.slice(addr, safe_len)
    }

    #[inline]
    pub fn copy_from_slice(&mut self, data: &[u8]) {
        assert!(data.len() <= self.len());
        self.0.copy_from_slice(data);
    }

    #[inline]
    pub fn slice_n<const N: usize>(&self, start: usize) -> &[u8; N] {
        self.0.as_slice()[start..start + N].try_into().unwrap()
    }

    #[inline]
    pub fn fill(&mut self, value: u8) {
        self.0.fill(value);
    }
}

impl<const S: usize> std::ops::Index<u16> for Mem<S> {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl<const S: usize> std::ops::IndexMut<u16> for Mem<S> {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl<const S: usize> std::ops::Index<usize> for Mem<S> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<const S: usize> std::ops::IndexMut<usize> for Mem<S> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<const N: usize> serde::Serialize for Mem<N> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_bytes(self.as_slice())
    }
}

impl<'de, const N: usize> serde::Deserialize<'de> for Mem<N> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<const N: usize>;
        impl<'de, const N: usize> serde::de::Visitor<'de> for Visitor<N> {
            type Value = Box<[u8; N]>;
            fn expecting(
                &self,
                formatter: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                write!(formatter, "memory buffer of len {N}")
            }
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut arr = Vec::with_capacity(N);
                while let Some(item) = seq.next_element()? {
                    arr.push(item);
                }
                arr.try_into().map_err(|arr: Vec<u8>| {
                    serde::de::Error::invalid_length(arr.len(), &self)
                })
            }
            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.try_into().map_err(|arr: Vec<u8>| {
                    serde::de::Error::invalid_length(arr.len(), &self)
                })
            }
        }
        deserializer.deserialize_byte_buf(Visitor::<N>).map(Mem)
    }
}
